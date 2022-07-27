{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Runic.Context (Context, Item(..)) where

import           Runic.Types                    ( Polytype)
import           Data.Text                      ( Text )

type Identifier = Text
type Context = [Item]

newtype EId = EId Int
  deriving (Show, Eq)

data Item :: * where
  -- | Some type variable \alpha
  Var          :: Identifier -> Item
  -- | A variable annotated with a type (x : A)
  Annotation   :: Identifier -> Polytype -> Item
  -- | A solved existential type variable (\vec{\alpha} -> \tau)
  EVar         :: Identifier -> Polytype -> Item
  -- | An unsolved existential variable (\vec{\alpha})
  UnsolvedEVar :: Identifier -> Item
  -- | Marker (weird triangle \vec{alpha})
  Marker       :: Identifier -> Item
  deriving (Show, Eq)
