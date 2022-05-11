module Interpreter
    ( run1
    ) where

import           Control.Exception              ( assert )
import           Control.Monad                  ( foldM
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT )
import qualified Control.Monad.Except          as E
import qualified Control.Monad.IO.Class        as IO
import           Control.Monad.State.Strict     ( StateT )
import qualified Control.Monad.State.Strict    as S
import qualified Data.List                     as L
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Parser                         ( BinOp(..)
                                                , Decl(..)
                                                , DoStatement(..)
                                                , Expr(..)
                                                , Identifier
                                                , Parameter(Parameter)
                                                , Value(..)
                                                )
import           Prelude                 hiding ( id )
import           Text.Pretty.Simple             ( pPrint )
import           Text.Printf                    ( printf )
import           Util

data Var = E Expr | F [Identifier] Expr
    deriving (Show)

data InterpreterError
    = NotInScope String
    | TypeError String
    | InternalError String
    | MissingMain
    | MainNotAFunction
    deriving (Show)

type Env = Map.Map Identifier Var

newtype Interpreter a = Interpreter (ExceptT InterpreterError (StateT Env IO) a)
    deriving (Monad, Functor, Applicative, E.MonadError InterpreterError, S.MonadState Env, IO.MonadIO)

runInterpreterT :: Interpreter a -> Env -> IO (Either InterpreterError a, Env)
runInterpreterT (Interpreter i) = S.runStateT $ E.runExceptT i

externs :: [(Identifier, Value -> Interpreter Value)]
externs = [("print", (>> pure IUnit) . pPrint)]

run1 :: [Decl] -> IO (Either InterpreterError Value)
run1 decls = do
    let env = setup decls Map.empty
    case Map.lookup "main" env of
        Just (F _ expr) -> fst <$> runInterpreterT (run expr) env
        Just (E _     ) -> pure $ Left MainNotAFunction
        Nothing         -> pure $ Left MissingMain

setup :: [Decl] -> Env -> Env
setup [] e = e
setup ((Let id _ expr) : xs) e             = setup xs $ Map.insert id (E expr) e
setup ((Extern id _ _) : xs) e             = setup xs $ Map.insert id (E . Val $ IExtern id) e
setup ((Function id params _ expr) : xs) e = setup xs e'
  where
    e'  = Map.insert id (F ids expr) e
    ids = map (\(Parameter i _) -> i) params

runIntOp :: (Int -> Int -> a) -> String -> (a -> Value) -> Expr -> Expr -> Interpreter Value
runIntOp f name ctor lhs rhs = do
    lhs' <- run lhs
    rhs' <- run rhs

    case (lhs', rhs') of
        (IInt a, IInt b) -> pure $ ctor (a `f` b)
        (a     , b     ) -> throw . TypeError $ printf
            "Got %s and %s on operation %s expecting int and int!"
            (typeStr a)
            (typeStr b)
            name

typeStr :: Value -> Text
typeStr (IInt    _) = "int"
typeStr (IBool   _) = "bool"
typeStr (IString _) = "str"
typeStr (IFunc _ _) = "fun"
typeStr (IFloat _ ) = "float"
typeStr IUnit       = "unit"
typeStr (IExtern _) = "extern"

notBoolErr :: Text -> Text -> Text -> Interpreter a
notBoolErr a b c = throw $ TypeError (printf "Expected boolean to %s of %s, got %s" a b c)

run :: Expr -> Interpreter Value
run (Val v)         = pure v
run UnitLit         = pure IUnit
run (IntLit   i   ) = pure $ IInt i
run (FloatLit f   ) = pure $ IFloat f
run (BoolLit  b   ) = pure $ IBool b

run (If cond e1 e2) = do
    cond' <- run cond

    b     <- case cond' of
        IBool bool -> pure bool
        _          -> throw $ TypeError "If has non-bool in conditional!"

    if b then run e1 else run e2

run (Lambda params ret expr) = pure $ IFunc (map (\(Parameter n _) -> n) params) expr

run (Neg e                 ) = do
    e' <- run e

    case e' of
        IInt   i -> pure $ IInt (-i)
        IFloat i -> pure $ IFloat (-i)
        _        -> throw $ TypeError "tried to negate non-float/int"

run (Do exprs) = do
    assert (not . null $ exprs) $ pure ()

    let run' = \case
            DoLet id ty e -> do
                S.modify (Map.insert id (E e))
                pure IUnit
            DoExpr e -> run e

    locally (const $ foldM (const run') undefined exprs)

run (Call f paramV) = do
    f' <- run f

    case f' of
        IFunc paramN expr -> do
            m       <- S.get
            paramV' <- mapM run paramV

            let params = zip paramN (map (E . Val) paramV')
            let m'     = Map.union (Map.fromList params) m

            S.put m'
            v <- run expr
            S.put m

            pure v
        --
        IExtern i -> do
            ef <- case L.find ((== i) . fst) externs of
                Nothing -> throw . NotInScope $ printf "Extern function %s does not exist!" i
                Just ef -> pure $ snd ef

            when (length paramV /= 1) (throw $ InternalError "unimplemented: multi-args externs!")

            run (head paramV) >>= ef
        _ -> throw $ TypeError "Tried to call a non-function!"

run (Identifier i) = do
    m <- S.get

    v <- case Map.lookup i m of
        Just v  -> pure v
        Nothing -> throw $ NotInScope ("Variable " <> T.unpack i <> " not in scope!")

    case v of
        E expr        -> run expr
        F params expr -> pure $ IFunc params expr

run (StringLiteral t             ) = pure $ IString t
run (Operator LessThan    lhs rhs) = runIntOp (<) "<" IBool lhs rhs
run (Operator GreaterThan lhs rhs) = runIntOp (>) ">" IBool lhs rhs
run (Operator LeEqTo      lhs rhs) = runIntOp (<=) "<=" IBool lhs rhs
run (Operator GrEqTo      lhs rhs) = runIntOp (>=) ">=" IBool lhs rhs
run (Operator Add         lhs rhs) = runIntOp (+) "+" IInt lhs rhs
run (Operator Sub         lhs rhs) = runIntOp (-) "-" IInt lhs rhs
run (Operator Mul         lhs rhs) = runIntOp (*) "*" IInt lhs rhs
run (Operator Div         lhs rhs) = runIntOp div "/" IInt lhs rhs
run (Operator EqualTo     lhs rhs) = (IBool ... (==)) <$> run lhs <*> run rhs
run (Operator And         lhs rhs) = do
    lhs' <- run lhs

    case lhs' of
        IBool False -> pure $ IBool False
        IBool True  -> run rhs >>= \case
            IBool b -> pure $ IBool b
            a       -> notBoolErr "rhs" "and" (typeStr a)
        a -> notBoolErr "lhs" "and" (typeStr a)
run (Operator Or lhs rhs) = do
    lhs' <- run lhs

    case lhs' of
        IBool True  -> pure $ IBool True
        IBool False -> run rhs >>= \case
            IBool b -> pure $ IBool b
            a       -> notBoolErr "rhs" "or" $ typeStr a
        a -> notBoolErr "lhs" "or" $ typeStr a
