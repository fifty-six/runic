{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Runic.Typecheck () where

import           Control.Monad.Except           ( ExceptT
                                                , MonadError()
                                                
                                                )
import qualified Control.Monad.Except          as E
import           Control.Monad.State.Class      ( MonadState )
import           Control.Monad.State.Strict     ( State )
import qualified Control.Monad.State.Strict    as S
import           Data.Text                      ( Text )
import           Runic.Parser                   ( Identifier
                                                
                                                )
import qualified Runic.Parser                  as P
import           Prelude                 hiding ( id
                                                , lookup
                                                )
import           Runic.Util
import           Runic.Context                  ( Context )
import qualified Runic.Context                 as Ctx
import Runic.Types (Type(..), Polytype)
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
               | Internal Text

type Env = Context

newtype Semant a = Semant (ExceptT TypeError (State Env) a)
    deriving (Functor, Applicative, Monad, MonadState Env, MonadError TypeError)

hole :: Eq a => a -> [a] -> ([a], [a])
hole a = second tailS . break (== a)
  where
  tailS (x:xs) = xs
  tailS [] = []

wellFormed :: Context -> Type a -> Semant ()
wellFormed gamma ty =
    case ty of
        -- UnitWF
        Unit -> pure ()

        -- Similar to UnitWF, Scalars are well-formed.
        Scalar _ -> pure ()

        -- Check that the type being pointed to is wf 
        Pointer p -> wellFormed gamma p

        -- UvarWF
        Var id | Ctx.Var id `elem` gamma -> return ()
               | otherwise       -> throw $ TypeNotInScope id

        -- ArrowWF
        -- Check that both the input and outputs are well-formed.
        Fn from to -> do
            wellFormed gamma from
            wellFormed gamma to

        -- ForallWF, wf if ty' is wf in the extended context
        -- i.e. \forall a. b, -> b is wf is it's wf with a in the context 
        ForAll alpha ty'  -> wellFormed (Ctx.Var alpha : gamma) ty'

        -- Well-formed if we can put the existential in the context.
        -- or solved??
        Exists alpha' -> wellFormed (Ctx.Var alpha' : gamma) ty

solve :: Context -> Polytype -> Maybe Polytype
solve gamma (Exists a) = 
    case filter isSol gamma of
        [Ctx.EVar id ty] -> Just ty
        []               -> Nothing
        x                -> error $ "Internal error, multiple solutions to existential. " ++ show x

    where
    isSol (Ctx.EVar id ty) = id == a
    isSol _ = False
-- I should probably make this type safe but that's so much effort
solve gamma _ = error "tried to solve non-existential"

substituteCtx :: Context -> Polytype -> Polytype
substituteCtx gamma alpha = case alpha of
        Exists a -> maybe alpha (substituteCtx gamma) (solve gamma alpha)

        ForAll a ty -> ForAll a $ substituteCtx gamma ty

        Pointer p -> substituteCtx gamma p

        Fn a b -> Fn (substituteCtx gamma a) (substituteCtx gamma b)

        -- Just leave these unchanged
        Unit      -> alpha
        Scalar _  -> alpha
        Var _     -> alpha

instL gamma a b = undefined
