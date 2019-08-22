{-# LANGUAGE OverloadedStrings #-}


-- | Support for making load-time configuration values available at run-time.

module Language.Avaleryar.Native.Config
  (
   configDb
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Map as Map
import Data.Text (Text)

import Language.Avaleryar.Syntax
import Language.Avaleryar.Semantics

configDb :: (Factual a, MonadIO m) => Maybe Text -> [a] -> NativeDb m
configDb sub = mkNativeDb (maybe "config" ("config/" <> ) sub) . combineFacts

combineFacts :: (MonadIO m, Factual a) => [a] -> [NativePred m]
combineFacts as = toList nps
  where nps = Map.fromListWith go [(p, mkNativeFact f) | f@(Lit p _) <- toFact <$> as ]
        go (NativePred np m) (NativePred np' _) = NativePred (\args -> np args <|> np' args) m -- the @m@s are the same due to 'mkNativePred'
