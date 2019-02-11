{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module SemanticsSpec where

import Data.Foldable
import System.FilePath
import System.Timeout

import Language.Avaleryar.Parser
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax

import Test.Hspec

shouldSucceed, shouldFail :: (HasCallStack, Show a) => IO [a] -> Expectation
shouldSucceed io = io >>= (`shouldNotSatisfy` null)
shouldFail    io = io >>= (`shouldSatisfy`    null)

spec :: Spec
spec = do
  describe "infinite loops" $ do
    it "doesn't run forever" $ do
      let go = runAvalaryarT 5000 1 testDb q
          q  = query "system" "loop" [Var "x"]
      timeoutSecs 1 go `shouldReturn` Just []

    it "demonstrates fair conjunction" $ do
      answers <- queryFile (exampleDir </> "fair-conjunction.ava") [qry| a(?x) |]
      answers `shouldNotSatisfy` null

    it "demonstrates fair disjunction" $ do
      answers <- queryFile (exampleDir </> "fair-disjunction.ava") [qry| a(?x) |]
      answers `shouldNotSatisfy` null

    it "finds paths" $ do
      let go = queryFile (exampleDir </> "path.ava")

      -- verbose for errors with line numbers
      shouldSucceed $ go [qry| path(1, 2) |]
      shouldSucceed $ go [qry| path(1, 3) |]
      shouldSucceed $ go [qry| path(1, 4) |]
      shouldSucceed $ go [qry| path(1, 5) |]
      shouldSucceed $ go [qry| path(2, 1) |]
      shouldSucceed $ go [qry| path(2, 3) |]
      shouldSucceed $ go [qry| path(2, 4) |]
      shouldSucceed $ go [qry| path(2, 5) |]
      shouldSucceed $ go [qry| path(3, 1) |]
      shouldSucceed $ go [qry| path(3, 2) |]
      shouldSucceed $ go [qry| path(3, 4) |]
      shouldSucceed $ go [qry| path(3, 5) |]
      shouldSucceed $ go [qry| path(5, 4) |]

      shouldFail    $ go [qry| path(4, 1) |]
      shouldFail    $ go [qry| path(4, 2) |]
      shouldFail    $ go [qry| path(4, 3) |]
      shouldFail    $ go [qry| path(4, 5) |]
      shouldFail    $ go [qry| path(5, 1) |]
      shouldFail    $ go [qry| path(5, 2) |]
      shouldFail    $ go [qry| path(5, 3) |]

exampleDir :: FilePath
exampleDir = "test/examples"

testRulesDb :: RulesDb IO
testRulesDb = insertRuleAssertion "system" rm mempty
  where rm = compileRules . fmap (fmap unRawVar) $ [rls| loop(?x) :- loop(?x). |]

testNativeDb :: NativeDb IO
testNativeDb = mempty

testDb :: Db IO
testDb = Db testRulesDb testNativeDb

timeoutSecs :: Int -> IO a -> IO (Maybe a)
timeoutSecs n = timeout $ n * 10 ^ 6

queryFile :: HasCallStack => FilePath -> Lit TextVar -> IO [Lit EVar]
queryFile p q = do
  Right rs <- parseFile p (Just $ const "system")
  let rdb = insertRuleAssertion "system" rm mempty
      rm  = compileRules . fmap (fmap unRawVar) $ rs
      go  = runAvalaryarT 500 10 (Db rdb testNativeDb) $ query' "system" q

  Just res <- timeoutSecs 5 go
  pure res
