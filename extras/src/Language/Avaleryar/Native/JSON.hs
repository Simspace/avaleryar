{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- | Native avaleryar predicates for working with JSON

module Language.Avaleryar.Native.JSON
  ( JwtConfig(..)
  , unsecuredJwtConfig
  , db
  , allClaims
  , securedClaims
  , unsecuredClaims
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson                 (fromJSON)
import qualified Data.Aeson                 as Aeson
import qualified Data.Attoparsec.Text       as AT
import           Data.Bifunctor             (first)
import           Data.Foldable              (toList)
import           Data.JSONPath
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           GHC.Generics               (Generic)
import qualified Jose.Jwa                   as Jwt
import           Jose.Jwk
import           Jose.Jwt                   hiding (decode)
import qualified Jose.Jwt                   as Jwt

import Language.Avaleryar.Instances ()
import Language.Avaleryar.Semantics (NativeDb(..), mkNativeDb, mkNativePred)
import Language.Avaleryar.Syntax    (Value(..))

runJsonPath :: Text -> Aeson.Value -> Either String [Value]
runJsonPath path val = do
  pes <- AT.parseOnly jsonPath path
  fmap (foldMap toList . fmap fromJSON) $ executeJSONPathEither pes val

runJsonPath' :: Text -> Aeson.Value -> [Value]
runJsonPath' path val = concat $ runJsonPath path val

extractJsonPath :: Text -> Text -> [Value]
extractJsonPath path json = concat $ Aeson.eitherDecodeStrict (encodeUtf8 json) >>= runJsonPath path

data JwtConfig = JwtConfig
  { jwks        :: [Jwk]
  , jwtEncoding :: Maybe JwtEncoding
  } deriving (Eq, Show, Generic)

unsecuredJwtConfig :: JwtConfig
unsecuredJwtConfig = JwtConfig [] (Just $ JwsEncoding Jwt.None)

-- | Extracts the signed/encrypted content from a 'JwtContent', but failing on 'Unsecured' data.
securedJwtContent :: JwtContent -> Either String Aeson.Value
securedJwtContent (Jws (_, bs))  = Aeson.eitherDecodeStrict bs
securedJwtContent (Jwe (_, bs))  = Aeson.eitherDecodeStrict bs
securedJwtContent (Unsecured _)  = Left "content is unsecured"

-- | Extracts the 'Unsecured' content from a 'JwtContent'.
unsecuredJwtContent :: JwtContent -> Either String Aeson.Value
unsecuredJwtContent (Unsecured bs) = Aeson.eitherDecodeStrict bs
unsecuredJwtContent _              = Left "content is not unsecured"

-- | Extracts content from a 'JwtContent' regardless of its securedness.  Used for the
-- @unsecured-claim@ predicate.
allJwtContent :: JwtContent -> Either String Aeson.Value
allJwtContent content = unsecuredJwtContent content <|> securedJwtContent content

claims :: (JwtContent -> Either String Aeson.Value) -> JwtConfig -> Text -> Text -> IO [Value]
claims extractor (JwtConfig ks encoding) json path = do
  content <- Jwt.decode ks encoding (encodeUtf8 json)
  pure $ toList (first show content >>= extractor) >>= runJsonPath' path -- TODO: Suck less

-- | Underlying implementation of the @unsecured-claim@ predicate.
allClaims :: JwtConfig -> Text -> Text -> IO [Value]
allClaims       = claims allJwtContent

-- | Underlying implementation of the @claim@ predicate.
securedClaims :: JwtConfig -> Text -> Text -> IO [Value]
securedClaims   = claims securedJwtContent

-- | As 'securedClaims', but **only** extracts unsecured claims---does not correspond to any
-- predicate.
unsecuredClaims :: JwtConfig -> Text -> Text -> IO [Value]
unsecuredClaims = claims unsecuredJwtContent

-- TODO: Implement a baked-in @expired@ check---this is implementable via @posix-time@ in @:base@
-- and @claim@, but it would be nicer to have it pre-canned.

-- | Creates a native database in an assertion named @:json@ that provides predicates for extracting
-- data from json values via jsonpath and for extracting claims from encoded JWTs similarly.
--
-- FIXME: Currently, configuring JWT decoding is a little crufty---the database needs to be
-- configured with a 'JwtConfig' that contains the data we nee to pass on to 'Jwt.decode'.  This is
-- irksomely static and rather inflexible, but will do for now.  For testing, we provide
-- 'unsecuredJwtConfig' which will allow the decrypting of unsecured JWTs, though they will only be
-- accessible via the @unsecured-claim@ predicate.  Hopefully this is sufficiently awkward to
-- discourage brazenly insecure use of this capability.
db :: MonadIO m => JwtConfig -> NativeDb m
db jc = mkNativeDb "json" [ mkNativePred "path" extractJsonPath
                          , mkNativePred "claim" $ claims securedJwtContent jc
                          , mkNativePred "unsecured-claim" $ claims allJwtContent jc]



