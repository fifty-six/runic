{-# LANGUAGE NamedFieldPuns #-}

module Runic.Typecheck () where

import           Control.Monad                  ( forM
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
import qualified Runic.Parser                  as P
import           Prelude                 hiding ( id
                                                , lookup
                                                )
import           Text.Printf                    ( printf )
import           Runic.Util
import           Runic.Context                  ( Context )
import qualified Runic.Context                 as Ctx
import           Data.Maybe                     ( isJust )
import Runic.Types (Type(..), Scalar(..), Polytype)
import qualified Data.List as L
import Control.Arrow (second)

type PType = P.Type

-- data Type = I32 
--           | Unit 
--           | Bool 
--           | String 
--           | Char
--           | F32 
--           | Func [Type] Type 
--           | Generic Identifier Type 
--           | Pointer Type
--     deriving (Eq, Show)

data TypeError = TypeNotInScope Identifier

-- data SemantError = IdentifierNotInScope { var :: Text, varExpr :: Expr }
--                  | TypeNotInScope { tBind :: Maybe Text, tVar :: Text, tExpr :: Maybe Expr }
--                  | Internal Text
--                  | NotEnoughArguments { callExpr :: Expr, expectedCt :: Int, gotCt :: Int }
--                  | NoMain
--                  | TypeError { expected :: Type, got :: Type, containingExpr :: Maybe Expr, errorExpr :: Expr }
--                  | MismatchedArms { tArm1 :: Type, tArm2 :: Type, arm1E :: Expr, arm2E :: Expr }
--                  | NotAFunction { fnExpr :: Expr, callExpr :: Expr }
--                  | DuplicateDeclarationError { decl :: Text }
--                  deriving Show

type Env = Context

newtype Semant a = Semant (ExceptT TypeError (State Env) a)
    deriving (Functor, Applicative, Monad, MonadState Env, MonadError TypeError)

-- type SExpr = (Type, SExpr')
-- data SExpr'
--     = SStringLiteral Text
--     | SIntLit Int
--     | SFloatLit Float
--     | SBoolLit Bool
--     | SUnitLit
--     | SOperator BinOp SExpr SExpr
--     | SIdentifier Text
--     | SCall SExpr [SExpr]
--     | SNeg SExpr
--     | SIf SExpr SExpr SExpr
--     | SLambda [SParameter] Type SExpr
--     | SDo [SExpr]
--     | SDoLet Identifier Type SExpr
--     deriving (Show, Eq)
-- 
-- data SParameter = SParameter Identifier Type
--     deriving (Show, Eq)
-- 
-- data SDecl
--     = SLet Identifier Type SExpr
--     | SFunction Identifier [SParameter] Type SExpr
--     | SExtern Identifier [SParameter] Type
--     deriving (Show, Eq)

hole :: Eq a => a -> [a] -> ([a], [a])
hole a = second tailS . break (== a)
  where 
  tailS (x:xs) = xs
  tailS [] = []

wellFormed :: Context -> Type a -> Semant ()
wellFormed gamma ty = do
    case ty of
        -- UnitWF
        Unit -> pure ()

        -- Similar to UnitWF, Scalars are well-formed.
        Scalar _ -> pure ()

        -- As are pointers
        Pointer _ -> pure ()

        -- UvarWF
        Var id | Ctx.Var id `elem` gamma -> return ()
               | otherwise       -> throw $ TypeNotInScope id

        -- ArrowWF
        Fn from to -> do
            wellFormed gamma from
            wellFormed gamma to

        -- ForallWF
        ForAll alpha ty'  -> wellFormed (Ctx.Var alpha : gamma) ty

        Exists alpha' -> wellFormed (Ctx.Var alpha' : gamma) ty

substituteCtx :: Context -> Polytype -> Polytype
substituteCtx gamma alpha@(Exists a) = maybe alpha (\(Ctx.EVar id ty) -> ty) (L.find canSub gamma)
  where
  canSub (Ctx.EVar id ty) = id == a
  canSub _ = False
