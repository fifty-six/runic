module Typecheck
    ( typecheck
    , runSemant
    ) where

{-# LANGUAGE RecordWildCards #-}

import           Control.Exception              ( assert )
import           Control.Monad                  ( foldM
                                                , forM
                                                , forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                )
import qualified Control.Monad.Except          as E
import           Control.Monad.State.Class      ( MonadState )
import           Control.Monad.State.Strict     ( State )
import qualified Control.Monad.State.Strict    as S
import qualified Data.Functor.Identity         as Id
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Parser                         ( BinOp(..)
                                                , Decl(..)
                                                , Expr(..)
                                                , Parameter(Parameter)
                                                )
import           Prelude                 hiding ( id )
import           Text.Printf                    ( printf )

data Type = I32 | Unit | Bool | String | F32 | Func [Type] Type
    deriving (Eq, Show)

data SemantError = Invalid Text
                 | IdentifierNotInScope { var :: Text, varExpr :: Expr }
                 | TypeNotInScope { tBind :: Maybe Text, tVar :: Text }
                 | Internal Text
                 | NoMain
                 | TypeError { expected :: Type, got :: Type, containingExpr :: Expr, errorExpr :: Expr }
                 | MismatchedArms { tArm1 :: Type, tArm2 :: Type, arm1E :: SExpr, arm2E :: SExpr }
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
    | SDo [SExpr]
    deriving (Show, Eq)

type Identifier = Text
data SParameter = SParameter Identifier Type
    deriving (Show, Eq)

data SDecl
    = SLet Identifier Type SExpr
    | SFunction Identifier [SParameter] Type SExpr
    | SExtern Identifier [SParameter] Type
    deriving (Show, Eq)

throw :: MonadError e m => e -> m a
throw = throwError

intOps :: [BinOp]
intOps = [Add, Sub, Mul, Div]

ordOps :: [BinOp]
ordOps = [LeEqTo, GrEqTo, LessThan, GreaterThan]

boolOps :: [BinOp]
boolOps = [And, Or]

opRes :: M.Map BinOp Type
opRes = M.fromList
    [ (And        , Bool)
    , (Or         , Bool)
    , (LeEqTo     , Bool)
    , (GrEqTo     , Bool)
    , (EqualTo    , Bool)
    , (LessThan   , Bool)
    , (GreaterThan, Bool)
    , (Add        , I32)
    , (Sub        , I32)
    , (Mul        , I32)
    , (Div        , I32)
    ]

builtinTypes :: M.Map Text Type
builtinTypes =
    M.fromList [("i32", I32), ("f32", F32), ("str", String), ("unit", Unit), ("bool", Bool)]

runSemant :: Semant a -> Env -> (Either SemantError a, Env)
runSemant (Semant s) e = Id.runIdentity . flip S.runStateT e $ E.runExceptT s

lookupTy :: Maybe Text -> Text -> Semant Type
lookupTy n t = do
    case M.lookup t builtinTypes of
        Just a  -> pure a
        Nothing -> throw $ TypeNotInScope { tBind = n, tVar = t }

parseParams :: Traversable t => t Parameter -> Semant (t Type)
parseParams = mapM (\(Parameter n t) -> lookupTy (Just n) t)

typecheck :: [Decl] -> Semant [SDecl]
typecheck decls = do
    let addF s p r = do
            params <- parseParams p
            ret    <- lookupTy Nothing r
            S.modify (M.insert s $ Func params ret)

    forM_ decls $ \case
        Function sym params ret _ -> addF sym params ret
        Extern sym params ret     -> addF sym params ret
        Let    sym ty     _       -> lookupTy (Just sym) ty >>= S.modify . M.insert sym

    forM decls $ \decl -> do
        orig <- S.get
        case decl of
            Function _ params _ _ -> do
                params' <- parseParams params

                forM_ (zip params params') $ \(Parameter n _, t) -> S.modify $ M.insert n t

            _ -> pure ()

        decl' <- typecheckDecl decl
        S.put orig

        pure decl'

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
        throw $ TypeError { got = te, expected = ret', containingExpr = e, errorExpr = e }

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
typecheck' (Val           _) = throw $ Invalid "Shouldn't have a value!"
typecheck' (StringLiteral s) = pure (String, SStringLiteral s)
typecheck' (IntLit        i) = pure (I32, SIntLit i)
typecheck' (FloatLit      f) = pure (F32, SFloatLit f)
typecheck' (BoolLit       b) = pure (Bool, SBoolLit b)
typecheck' UnitLit           = pure (Unit, SUnitLit)

typecheck' e@(Neg i)         = do
    (i', ti) <- check i

    unless (ti == I32) $ do
        throw TypeError { expected = I32, got = ti, errorExpr = i, containingExpr = e }

    pure (ti, SNeg i')

typecheck' e@(Operator op lhs rhs) = do
    (lhs', tlhs) <- check lhs
    (rhs', trhs) <- check rhs

    unless (tlhs == trhs) $ do
        throw TypeError { expected = tlhs, got = trhs, errorExpr = rhs, containingExpr = e }

    let assertOfType t = unless (tlhs == t) $ do
            throw TypeError { expected = t, got = tlhs, errorExpr = e, containingExpr = e }

    when (op `elem` intOps || op `elem` ordOps) $ do
        assertOfType I32

    when (op `elem` boolOps) $ do
        assertOfType Bool

    tOp <- maybe (error . printf "Invalid operator %s!" $ show op) pure $ M.lookup op opRes

    pure (tOp, SOperator op lhs' rhs')

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

    -- Now we need to check our params are well-formed.
    m       <- S.get
    params' <- forM (zip params fps) $ \(p, fp_t) -> do
        (p', tp) <- check p

        unless (tp == fp_t) $ do
            throw TypeError { expected = fp_t, got = tp, errorExpr = p, containingExpr = e }

        pure p'

    pure (ret, SCall fn' params')

typecheck' e@(If cond arm1 arm2) = do
    (cond', tcond) <- check cond

    unless (tcond == Bool) $ do
        throw $ TypeError { expected = Bool, got = tcond, errorExpr = cond, containingExpr = e }

    (arm1E, tArm1) <- check arm1
    (arm2E, tArm2) <- check arm2

    unless (tArm1 == tArm2) $ do
        throw $ MismatchedArms { .. }

    pure (tArm1, SIf cond' arm1E arm2E)

typecheck' e@(Do exprs) = do
    assert (not . null $ exprs) $ pure ()
    foldM (const typecheck') undefined exprs
