{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Avaleryar.Syntax where

import Data.Map    (Map)
import Data.String
import Data.Text   (Text)
import Data.Void

data Value = I Int
           | T Text
           | S Text -- symbol
           | B Bool
             deriving (Eq, Ord, Read, Show)

instance IsString Value where
  fromString = S . fromString

data Pred = Pred Text Int deriving (Eq, Ord, Read, Show)

data Term v = Val Value | Var v deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Lit v = Lit Pred [Term v] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ARef v = ARNative Text | ARTerm (Term v) deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- give unqualified body literals the "current" assertion
data BodyLit v = Says (ARef v) (Lit v)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Rule v = Rule (Lit v) [BodyLit v]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type Fact = Lit Void

newtype Epoch = Epoch { getEpoch :: Int }
  deriving (Eq, Ord, Read, Show, Num, Enum)

type TextVar = Text
type EVar = (Epoch, TextVar)
newtype RawVar = RawVar { unRawVar :: Text }
  deriving (Eq, Ord, Read, Show, IsString)


type Env = Map EVar (Term EVar)
