{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}


module PDPSpec where

import Data.Tree (Tree(Node))

import Language.Avaleryar.Parser
import Language.Avaleryar.PDP
import Language.Avaleryar.PDP.Handle as Hdl
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax

import Test.Hspec

ndb :: NativeDb
ndb = mkNativeDb "test" preds
  where preds = [ mkNativePred "range" $ \f t -> [I x | x <- [f..t]]
                , mkNativePred "boom"  $ [T "tick", "tick", "tick", error "boom"]]

spec :: Spec
spec = do
  describe "pdp configuration" $ do
    it "respects maxAnswers and maxDepth" $ do
      let conf  = either (error . show) id $ pdpConfigText ndb "foo(?x) :- :test says range(1, 20, ?x)."
          ans n = [(Lit (Pred "foo" 1) [Val $ I x], []) | x <- [1..n]]
          rq    = runQuery' [] [qry| foo(?abc) |]

      -- These feel a bit brittle.  Caveat lector.
      runPDP' rq conf                      `shouldReturn` Success (ans 10)
      runPDP' (withMaxAnswers 5 $ rq) conf `shouldReturn` Success (ans 5)
      runPDP' (withMaxDepth   1 $ rq) conf `shouldReturn` FuelExhausted
      runPDP' (withMaxDepth   2 $ rq) conf `shouldReturn` Success (ans 1)

      -- TODO: more tests

  describe "querying" $ do
    it "doesn't do extra work when (only) checking" $ do
      hdl <- either (error . show) newHandle $ pdpConfigText ndb "foo(?x) :- :test says boom(?x)."

      checkQuery   hdl [] "foo" [Val "tick"] `shouldReturn` Right True
      checkQuery   hdl [] "foo" [Val "boom"] `shouldThrow` errorCall "boom"
      Hdl.runQuery hdl [] "foo" [Val "tick"] `shouldThrow` errorCall "boom"
