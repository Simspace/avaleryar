{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}


module Language.Avaleryar.Instances where

import           Data.Aeson                           (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson                           as A
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import Language.Avaleryar.Syntax

instance ToJSON Value where
  toJSON (I i) = toJSON i
  toJSON (T t) = toJSON t
  toJSON (B b) = toJSON b

instance FromJSON Value where
  parseJSON (A.String s) = pure $ T s
  parseJSON (A.Bool b)   = pure $ B b
  parseJSON (A.Number n) = pure $ I (truncate n)       -- FIXME: Suck less
  parseJSON _            = fail "couldn't parse value" -- FIXME: Suck less

deriving newtype instance ToJSON   RawVar
deriving newtype instance FromJSON RawVar

instance ToJSON (Term RawVar) where
  toJSON (Val v) = toJSON v
  toJSON (Var v) = A.object ["var" .= v]

instance FromJSON (Term RawVar) where
  parseJSON (A.Object v) = Var <$> v .: "var"
  parseJSON v            = Val <$> parseJSON v

instance ToJSON (Lit RawVar) where
  toJSON (Lit (Pred p _) args) = A.object ["pred" .= p, "args" .= args]

instance FromJSON (Lit RawVar) where
  parseJSON = A.withObject "literal" $ \o -> do
                p    <- o .: "pred"
                args <- o .: "args"
                pure $ Lit (Pred p (length args)) args

instance ToField Value where
  toField = toJSONField

instance FromField Value where
  fromField = fromJSONField

instance ToField (Term RawVar) where
  toField = toJSONField

instance FromField (Term RawVar) where
  fromField = fromJSONField

-- | Encodes a 'Lit' @foo(bar, ?baz)@ as a @text@ column with @foo@ and a @jsonb@ column using the
-- aeson encoding of the argument list (@["bar", {"var":"baz"}]@).  I'm pretty sure logic
-- programming systems index off the entire functor (i.e., @foo/2@), but I figure maintaining the
-- right invariant on a length column probably isn't worth the effort when we could just as easily
-- use an expression index on the length of the argument list.
instance ToRow (Lit RawVar) where
  toRow (Lit (Pred p _) args) = toRow (p, toJSON args)

-- | Expects rows of the form @(text, jsonb)@ where the @jsonb@ is the same representation as the
-- aeson instance for 'Lit'.
instance FromRow (Lit RawVar) where
  fromRow = do
    p <- field
    args <- fieldWith fromJSONField
    pure $ Lit (Pred p (length args)) args
