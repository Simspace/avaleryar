{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Generally useful native predicates.

module Language.Avaleryar.Native.Base
  (
   db
  ) where

import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Text (Text, unpack, toLower, toUpper, strip)
import qualified Data.Text as Text

import Language.Avaleryar.Semantics (mkNativeDb, mkNativePred, Solely(..), NativeDb)
import Language.Avaleryar.Syntax (Value(..))

db :: MonadIO m => NativeDb m
db = mkNativeDb "base" [ mkNativePred "lt"  $ (<) @Value
                       , mkNativePred "gt"  $ (>) @Value
                       , mkNativePred "lt=" $ (<=) @Value
                       , mkNativePred "gt=" $ (>=) @Value
                       , mkNativePred "max" $ max @Value
                       , mkNativePred "min" $ min @Value
                       -- Arithmetic
                       , mkNativePred "add"  $ arith (+)
                       , mkNativePred "sub"  $ arith (-)
                       , mkNativePred "mul"  $ arith (*)
                       , mkNativePred "mod"  $ arith mod
                       , mkNativePred "abs"  $ I . abs
                       , mkNativePred "even" $ even @Int
                       , mkNativePred "odd"  $ odd @Int
                       -- Time
                       , mkNativePred "posix-time" posixTime
                       , mkNativePred "parse-time" parseTime
                       -- Text
                       , mkNativePred "length" $ Solely . Text.length
                       , mkNativePred "concat" $ (\t t' -> T $ t <> t') -- nearly looks like APL...
                       , mkNativePred "lc"     $ Solely . toLower
                       , mkNativePred "uc"     $ Solely . toUpper
                       , mkNativePred "strip"  $ Solely . strip
                       , mkNativePred "lines"  $ fmap Solely . Text.lines
                       , mkNativePred "words"  $ fmap Solely . Text.words
                       , mkNativePred "substr"      $ Text.isInfixOf
                       , mkNativePred "starts-with" $ Text.isPrefixOf
                       , mkNativePred "ends-with"   $ Text.isSuffixOf
                       ]

posixTime :: IO Value
posixTime = I . truncate <$> getPOSIXTime

parseTime :: Text -> Text -> Maybe Value
parseTime fs ts = do
  utc <- parseTimeM True defaultTimeLocale (unpack fs) (unpack ts)
  pure . I . truncate $ utcTimeToPOSIXSeconds utc

arith :: (Int -> Int -> Int) -> Int -> Int -> Value
arith f x y = I $ f x y
