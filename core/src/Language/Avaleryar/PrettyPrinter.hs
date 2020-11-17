{-# LANGUAGE TypeApplications #-}

module Language.Avaleryar.PrettyPrinter where

import           Data.Foldable
import qualified Data.Text                    as T
import           Debug.Trace                  (trace, traceM)
import           Text.PrettyPrint.Leijen.Text

import Language.Avaleryar.Semantics (RulesDb)
import Language.Avaleryar.Syntax

putQuery :: Lit TextVar -> IO ()
putQuery = putDoc . pretty

putFacts :: [Fact] -> IO ()
putFacts = traverse_ (putDoc . pretty . factToRule @TextVar)

putRulesDb :: RulesDb m -> IO ()
putRulesDb = putDoc . pretty

putAssertion :: Value -> [Pred] -> IO ()
putAssertion assn ps = putDoc $ prettyAssertion assn ps

traceP :: Pretty a => a -> b -> b
traceP = trace . T.unpack . displayTStrict . renderOneLine . pretty

traceMP :: (Pretty a, Applicative f) => a -> f ()
traceMP = traceM . T.unpack . displayTStrict . renderOneLine . pretty
