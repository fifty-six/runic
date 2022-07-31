-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
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
import Runic.Types (Type(..), Polytype, Monotype)
import Control.Arrow (second, Arrow (first))
import qualified Data.List as L

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

hole :: Eq a => a -> [a] -> Maybe ([a], [a])
-- | Break the list such that it returns a tuple containing the list 
-- | up to that element and then beyond, non-inclusive of the element.
hole a l = sequenceA $ second tailS . break (== a) $ l
  where
  tailS (x:xs) = Just xs
  tailS [] = Nothing

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

solve :: Context -> Identifier -> Monotype -> Maybe Monotype
solve gamma id t =
    case filter isSol gamma of
        [Ctx.EVar _ ty] -> Just ty
        []               -> Nothing
        x                -> error $ "Internal error, multiple solutions to existential. " ++ show x

    where
    isSol (Ctx.EVar id' ty) = id' == id
    isSol _ = False

apply :: Context -> Polytype -> Polytype
apply gamma alpha = case alpha of
        -- If we can solve the existential then replace that, otherwise just recur
        Exists a -> maybe alpha (apply gamma) (poly <$> (solve gamma a =<< mono alpha))

        ForAll a ty -> ForAll a $ apply gamma ty

        Pointer p -> apply gamma p

        Fn a b -> Fn (apply gamma a) (apply gamma b)

        -- Just leave these unchanged
        Unit      -> alpha
        Scalar _  -> alpha
        Var _     -> alpha

mono :: Type a -> Maybe Monotype
mono Unit       = pure Unit
mono (Var v)    = pure $ Var v
mono (ForAll _ _) = Nothing
mono (Exists v) = pure $ Exists v
mono (Fn a b)   = Fn <$> mono a <*> mono b
mono (Scalar s) = pure $ Scalar s
mono (Pointer p) = Pointer <$> mono p

-- Everything is a polytype
poly :: Type a -> Polytype
poly Unit       = Unit
poly (Var v)    = Var v
poly (ForAll v t) = ForAll v t 
poly (Exists v) = Exists v
poly (Fn a b)   = Fn (poly a) (poly b) 
poly (Scalar s) = Scalar s
poly (Pointer p) = Pointer (poly p)

-- instantiating alpha s.t. alpha is a subtype of t
instL :: Context -> Identifier -> Monotype -> Context
instL gamma alpha t = go
    where
    go | Just t' <- solve gamma alpha t
       , Just (g, g') <- hole (Ctx.UnsolvedEVar alpha) gamma 
       = undefined
       | otherwise = undefined
