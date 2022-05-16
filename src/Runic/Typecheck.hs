{-# LANGUAGE NamedFieldPuns #-}

module Runic.Typecheck
    ( typecheck
    , runSemant
    , SemantError(..)
    , Type(..)
    ) where

import           Control.Monad                  ( foldM
                                                , forM
                                                , forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError()
                                                , catchError
                                                )
import qualified Control.Monad.Except          as E
import           Control.Monad.State.Class      ( MonadState )
import           Control.Monad.State.Strict     ( State )
import qualified Control.Monad.State.Strict    as S
import qualified Data.Functor.Identity         as Id
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Runic.Parser                   ( BinOp(..)
                                                , Decl(..)
                                                , DoStatement(..)
                                                , Expr(..)
                                                , Identifier
                                                , Parameter(Parameter)
                                                )
import qualified Runic.Parser                   as P
import           Prelude                 hiding ( id
                                                , lookup
                                                )
import           Text.Printf                    ( printf )
import           Runic.Util

type PType = P.Type

data Type = I32 | Unit | Bool | String | F32 | Func [Type] Type
    deriving (Eq, Show)

data SemantError = IdentifierNotInScope { var :: Text, varExpr :: Expr }
                 | TypeNotInScope { tBind :: Maybe Text, tVar :: Text, tExpr :: Maybe Expr }
                 | Internal Text
                 | NotEnoughArguments { callExpr :: Expr, expectedCt :: Int, gotCt :: Int }
                 | NoMain
                 | TypeError { expected :: Type, got :: Type, containingExpr :: Maybe Expr, errorExpr :: Expr }
                 | MismatchedArms { tArm1 :: Type, tArm2 :: Type, arm1E :: Expr, arm2E :: Expr }
                 | NotAFunction { fnExpr :: Expr, callExpr :: Expr }
                 deriving Show

type Env = M.Map Text Type

newtype Semant a = Semant (ExceptT SemantError (State Env) a)
    deriving (Functor, Applicative, Monad, MonadState Env, MonadError SemantError)

type SExpr = (Type, SExpr')
data SExpr'
    = SStringLiteral Text
    | SIntLit Int
    | SFloatLit Float
    | SBoolLit Bool
    | SUnitLit
    | SOperator BinOp SExpr SExpr
    | SIdentifier Text
    | SCall SExpr [SExpr]
    | SNeg SExpr
    | SIf SExpr SExpr SExpr
    | SLambda [SParameter] Type SExpr
    | SDo [SExpr]
    deriving (Show, Eq)

data SParameter = SParameter Identifier Type
    deriving (Show, Eq)

data SDecl
    = SLet Identifier Type SExpr
    | SFunction Identifier [SParameter] Type SExpr
    | SExtern Identifier [SParameter] Type
    deriving (Show, Eq)

numOps :: [BinOp]
numOps = [Add, Sub, Mul, Div]

ordOps :: [BinOp]
ordOps = [LeEqTo, GrEqTo, LessThan, GreaterThan]

boolOps :: [BinOp]
boolOps = [And, Or]

builtinTypes :: M.Map PType Type
builtinTypes = M.fromList
    [ (P.Raw "i32" , I32)
    , (P.Raw "f32" , F32)
    , (P.Raw "str" , String)
    , (P.Raw "unit", Unit)
    , (P.Raw "bool", Bool)
    ]

runSemant :: Semant a -> Env -> (Either SemantError a, Env)
runSemant (Semant s) e = Id.runIdentity . flip S.runStateT e $ E.runExceptT s

lookupTy :: Maybe Text -> PType -> Maybe Expr -> Semant Type
lookupTy n t expr = do
    case t of
        P.Raw name -> case M.lookup t builtinTypes of
            Just a  -> pure a
            Nothing -> throw $ TypeNotInScope { tBind = n, tVar = name, tExpr = expr }
        P.FnTy params ret -> do
            let lookup l = lookupTy Nothing l Nothing
            params' <- mapM lookup params
            Func params' <$> lookup ret

addSym :: Text -> PType -> Expr -> Semant ()
addSym sym ty e = lookupTy (Just sym) ty (Just e) >>= S.modify . M.insert sym

parseParams :: Traversable t => t Parameter -> Semant (t Type)
parseParams = mapM (\(Parameter n t) -> lookupTy (Just n) t Nothing)

typecheck :: [Decl] -> Semant [SDecl]
typecheck decls = do
    let addF s p r expr = do
            params <- parseParams p
            ret    <- lookupTy Nothing r expr
            S.modify (M.insert s $ Func params ret)

    forM_ decls $ \case
        Function sym params ret e -> addF sym params ret (Just e)
        Extern sym params ret     -> addF sym params ret Nothing
        Let    sym ty     e       -> addSym sym ty e

    forM decls $ \decl -> locally $ \_ -> do
        case decl of
            Function _ params _ _ -> do
                params' <- parseParams params

                forM_ (zip params params') $ \(Parameter n _, t) -> S.modify $ M.insert n t

            _ -> pure ()

        typecheckDecl decl

internalLookupFunc :: Text -> [Parameter] -> Semant ([SParameter], Type)
internalLookupFunc id params = do
    let internalErr s = throw . Internal . T.pack . printf s . T.unpack

    f' <- S.gets $ M.lookup id
    f  <- case f' of
        Nothing -> internalErr "Couldn't find function %s" id
        Just f  -> pure f

    case f of
        Func paramsT ret -> do
            let params' = zipWith (\(Parameter n _) t -> SParameter n t) params paramsT
            pure (params', ret)
        _ -> internalErr "Lookup for %s didn't return function!" id


typecheckDecl :: Decl -> Semant SDecl
typecheckDecl (Function id params ret e) = do
    (params', ret') <- internalLookupFunc id params
    (e'     , te  ) <- check e

    unless (te == ret') $ do
        throw $ TypeError { got = te, expected = ret', containingExpr = Nothing, errorExpr = e }

    pure $ SFunction id params' ret' e'

typecheckDecl (Extern id params ret) = do
    (params', ret') <- internalLookupFunc id params

    pure $ SExtern id params' ret'

typecheckDecl (Let id t expr) = do
    (expr', texpr) <- check expr

    pure $ SLet id texpr expr'

check :: Expr -> Semant (SExpr, Type)
check a = do
    r@(tr, _) <- typecheck' a
    pure (r, tr)

typecheck' :: Expr -> Semant SExpr
-- Val should only be used by the interpreter.
typecheck' (Val           _) = throw $ Internal "Shouldn't have a value!"
typecheck' (StringLiteral s) = pure (String, SStringLiteral s)
typecheck' (IntLit        i) = pure (I32, SIntLit i)
typecheck' (FloatLit      f) = pure (F32, SFloatLit f)
typecheck' (BoolLit       b) = pure (Bool, SBoolLit b)
typecheck' UnitLit           = pure (Unit, SUnitLit)

typecheck' l@(Lambda p r e)  = do
    p' <- parseParams p
    r' <- lookupTy Nothing r (Just l)

    let ps = zipWith (\(Parameter n _) t -> SParameter n t) p p'

    (e', sr') <- locally $ \m -> do
        -- Need the parameters in scope while we're checking the body
        forM_ (zip p p') $ \(Parameter n _, t) -> S.modify $ M.insert n t

        check e

    unless (r' == sr') $ do
        throw $ TypeError { expected = r', got = sr', errorExpr = e, containingExpr = Just l }

    pure (Func p' r', SLambda ps r' e')

typecheck' e@(Neg i) = do
    (i', ti) <- check i

    unless (ti == I32) $ do
        throw TypeError { expected = I32, got = ti, errorExpr = i, containingExpr = Just e }

    pure (ti, SNeg i')

typecheck' e@(Operator op lhs rhs) = do
    (lhs', tlhs) <- check lhs
    (rhs', trhs) <- check rhs

    unless (tlhs == trhs) $ do
        throw TypeError { expected = tlhs, got = trhs, errorExpr = rhs, containingExpr = Just e }

    let assertOfType t = do
            unless (tlhs == t) $ do
                throw TypeError { expected       = t
                                , got            = tlhs
                                , errorExpr      = lhs
                                , containingExpr = Just e
                                }

            unless (trhs == t) $ do
                throw TypeError { expected       = t
                                , got            = trhs
                                , errorExpr      = rhs
                                , containingExpr = Just e
                                }

    if
        | op `elem` numOps -> do
            assertOfType I32 `catchError` const (assertOfType F32)
            pure (tlhs, SOperator op lhs' rhs')
        |
--
          op `elem` boolOps -> do
            assertOfType Bool
            pure (Bool, SOperator op lhs' rhs')
        |
--
          op `elem` ordOps -> do
            assertOfType I32 `catchError` const (assertOfType F32)
            pure (Bool, SOperator op lhs' rhs')
        |
--
          otherwise -> throw . Internal . T.pack $ printf "Invalid operator %s!" $ show op

typecheck' e@(Identifier i) = do
    m <- S.get

    t <- case M.lookup i m of
        Just t  -> pure t
        Nothing -> throw $ IdentifierNotInScope { var = i, varExpr = e }

    pure (t, SIdentifier i)

typecheck' e@(Call fn params) = do
    -- We just need to check that we have a function here
    (fn', tfn) <- check fn

    (fps, ret) <- case tfn of
        Func a b -> pure (a, b)
        _        -> throw NotAFunction { fnExpr = fn, callExpr = e }

    unless (length params == length fps) $ do
        throw NotEnoughArguments { callExpr = e, expectedCt = length fps, gotCt = length params }

    -- Now we need to check our params are well-formed.
    m       <- S.get
    params' <- forM (zip params fps) $ \(p, fp_t) -> do
        (p', tp) <- check p

        unless (tp == fp_t) $ do
            throw TypeError { expected = fp_t, got = tp, errorExpr = p, containingExpr = Just e }

        pure p'

    pure (ret, SCall fn' params')

typecheck' e@(If cond arm1 arm2) = do
    (cond', tcond) <- check cond

    unless (tcond == Bool) $ do
        throw $ TypeError { expected       = Bool
                          , got            = tcond
                          , errorExpr      = cond
                          , containingExpr = Just e
                          }

    (arm1', tArm1) <- check arm1
    (arm2', tArm2) <- check arm2

    unless (tArm1 == tArm2) $ do
        throw $ MismatchedArms { tArm1, tArm2, arm1E = arm1, arm2E = arm2 }

    pure (tArm1, SIf cond' arm1' arm2')

typecheck' e@(Do exprs) = do
    when (null exprs) $ do
        throw $ Internal "Got do block with no expressions!"

    let f _ = \case
            DoExpr d       -> typecheck' d
            DoLet id ty ex -> do
                t        <- lookupTy (Just id) ty (Just ex)
                (_, ty') <- check ex

                unless (t == ty') $ do
                    throw $ TypeError { got            = ty'
                                      , expected       = t
                                      , errorExpr      = ex
                                      , containingExpr = Just e
                                      }

                S.modify (M.insert id t)

                pure (Unit, SUnitLit)

    locally (const $ foldM f undefined exprs)
