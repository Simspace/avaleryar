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
import Test.QuickCheck (arbitrary, generate)

shouldSucceed, shouldFail, shouldNotTimeout :: (HasCallStack) => IO TestResult -> Expectation
shouldSucceed io    = io >>= (`shouldSatisfy` isSuccess)
  where isSuccess (Success _) = True
        isSuccess _           = False
shouldFail    io    = io `shouldReturn` Failure
shouldNotTimeout io = io `shouldNotReturn` Timeout

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

-- | TODO: Push this back into 'runAvaleryarT' or 'runM'...
data TestResult = Success [Lit EVar] | Failure | Timeout
  deriving (Eq, Ord, Read, Show)

testResult :: Maybe [Lit EVar] -> TestResult
testResult Nothing   = Timeout
testResult (Just []) = Failure
testResult (Just ss) = Success ss

queryRules :: HasCallStack => Lit TextVar -> [Rule RawVar] -> IO TestResult
queryRules q rs = do
  let rdb = insertRuleAssertion "qq" rm mempty
      rm = compileRules . fmap (fmap unRawVar) $ rs
      go = runAvalaryarT 500 10 (Db rdb testNativeDb) $ compileQuery' "qq" q

  testResult <$> timeoutSecs 1 go

queryFile :: HasCallStack => FilePath -> Lit TextVar -> IO TestResult
queryFile p q = do
  Right rs <- parseFile p (Just $ const "system")
  let rdb = insertRuleAssertion "system" rm mempty
      rm  = compileRules . fmap (fmap unRawVar) $ rs
      go  = runAvalaryarT 500 10 (Db rdb testNativeDb) $ compileQuery' "system" q

  testResult <$> timeoutSecs 1 go
