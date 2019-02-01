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

import Language.Avaleryar.Value (Value)

data Val = T Text | I Int deriving (Eq, Ord, Read, Show)

data Pred = Pred Text Int deriving (Eq, Ord, Read, Show)

data Term c v = Val c | Var v deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Lit c v = Lit Pred [Term c v] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ARef c v = ARNative Text | ARTerm (Term c v) deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- give unqualified body literals the "current" assertion
data BodyLit c v = Says (ARef c v) (Lit c v)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Rule c v = Rule (Lit c v) [BodyLit c v]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type Fact c = Lit c Void

newtype Epoch = Epoch { getEpoch :: Int }
  deriving (Eq, Ord, Read, Show, Num, Enum)

type TextVar = Text
type EVar = (Epoch, TextVar)

type Env c = Map EVar (Term c EVar)

