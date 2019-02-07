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
           | B Bool
             deriving (Eq, Ord, Read, Show)

instance IsString Value where
  fromString = T . fromString

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

factToRule :: Fact -> Rule v
factToRule lit = Rule (vacuous lit) []

newtype Epoch = Epoch { getEpoch :: Int }
  deriving (Eq, Ord, Read, Show, Num, Enum)

type TextVar = Text
type EVar = (Epoch, TextVar)
newtype RawVar = RawVar { unRawVar :: Text }
  deriving (Eq, Ord, Read, Show, IsString)

type Env = Map EVar (Term EVar)

data Mode v = In v | Out v
  deriving (Eq, Ord, Read, Show)

type ModedLit = Lit (Mode TextVar)

class Valuable a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

instance Valuable Value where
  toValue = id
  fromValue = Just

instance Valuable Text where
  toValue = T
  fromValue (T a) = Just a
  fromValue _     = Nothing
instance Valuable Int  where
  toValue = I
  fromValue (I a) = Just a
  fromValue _     = Nothing

instance Valuable Bool where
  toValue = B
  fromValue (B a) = Just a
  fromValue _     = Nothing

val :: Valuable a => a -> Term v
val = Val . toValue
