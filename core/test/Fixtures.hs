{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Fixtures where

import           Control.Monad
import           Data.Map        (Map)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.CPUTime
import           System.FilePath
import           System.Timeout

import Language.Avaleryar.Parser
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax

import Test.Hspec
import Test.QuickCheck

shouldSucceed, shouldFail :: (HasCallStack, Show a) => IO [a] -> Expectation
shouldSucceed io = io >>= (`shouldNotSatisfy` null)
shouldFail    io = io >>= (`shouldSatisfy`    null)

exampleDir :: FilePath
exampleDir = "test/examples"

exampleFile :: FilePath -> FilePath
exampleFile fp = exampleDir </> fp

testRulesDb :: RulesDb IO
testRulesDb = insertRuleAssertion "system" rm mempty
  where rm = compileRules . fmap (fmap unRawVar) $ [rls| loop(?x) :- loop(?x). |]

testNativeDb :: NativeDb IO
testNativeDb = mkNativeDb "prim" preds
  where preds = [ mkNativePred "not=" $ (/=) @Value -- lift bool to pred on 'Value'
                , mkNativePred "even" $ even @Int  -- lift bool to pred on 'Valuable Int'
                , mkNativePred "rev"  $ Solely . T.reverse     -- lift text transform to pred(+, -)
                , mkNativePred "lines" $ fmap Solely . T.lines -- lift list to multiple successes
                , mkNativePred "cpu-time" $ cpuTime            -- lift IO to pred
                , mkNativePred "silly"    $ silly              -- lift IO list to multiple successes
                ]
        silly :: Int -> IO [(Int, Bool)]
        silly n = replicateM n (generate @(Int, Bool) arbitrary)

        cpuTime :: IO (Solely Int)
        cpuTime = Solely . fromInteger <$> getCPUTime

testNativeModes :: Map Text (Map Pred ModedLit)
testNativeModes = fmap (fmap nativeSig) . unNativeDb $ testNativeDb

testDb :: Db IO
testDb = Db testRulesDb testNativeDb

timeoutSecs :: Int -> IO a -> IO (Maybe a)
timeoutSecs n = timeout $ n * 10 ^ (6 :: Int)

queryRules :: HasCallStack => Lit TextVar -> [Rule RawVar] -> IO [Lit EVar]
queryRules q rs = do
  let rdb = insertRuleAssertion "qq" rm mempty
      rm = compileRules . fmap (fmap unRawVar) $ rs
      go = runAvalaryarT 500 10 (Db rdb testNativeDb) $ compileQuery' "qq" q

  Just res <- timeoutSecs 1 go
  pure res

queryFile :: HasCallStack => FilePath -> Lit TextVar -> IO [Lit EVar]
queryFile p q = do
  Right rs <- parseFile p (Just $ const "system")
  let rdb = insertRuleAssertion "system" rm mempty
      rm  = compileRules . fmap (fmap unRawVar) $ rs
      go  = runAvalaryarT 500 10 (Db rdb testNativeDb) $ compileQuery' "system" q

  Just res <- timeoutSecs 1 go
  pure res
