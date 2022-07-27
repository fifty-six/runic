{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Runic.Types
    ( Type(..)
    , TKind
    , Monotype
    , Polytype
    , Scalar(..)
    ) where
import           Data.Text                      ( Text )

type Identifier = Text

data TKind = Mono | Poly

data Type :: TKind -> * where
  -- | ()
  Unit   :: Type a
  -- | A -> B
  Fn     :: Type a -> Type a -> Type a
  -- | ForAll alpha. A
  ForAll :: Identifier -> Type Poly -> Type Poly
  -- | alpha
  Var    :: Identifier -> Type a
  -- | \vec{\alpha}
  Exists :: Identifier -> Type a
  -- | Scalar
  Scalar :: Scalar -> Type a
  -- | Pointer
  Pointer :: Type a -> Type a
deriving instance Show (Type a)
deriving instance Eq (Type a)

data Scalar = Integer { width :: Int }
            | Float   { width :: Int }
            | Bool
            | String
            deriving (Show, Eq)

type Monotype = Type Mono
type Polytype = Type Poly
