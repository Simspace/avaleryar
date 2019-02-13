{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module SemanticsSpec where

import Control.Monad

import Language.Avaleryar.Parser
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax

import Fixtures

import Test.Hspec

spec :: Spec
spec = do
  describe "infinite loops" $ do
    it "don't run forever" $ do
      let go = runAvalaryarT 5000 1 testDb q
          q  = query "system" "loop" [Var "x"]
      timeoutSecs 1 go `shouldReturn` Just []

    it "have limited output" $ do
      Just answers <- timeoutSecs 1 $ runAvalaryarT 5000 10 testDb (msum . replicate 74 $ pure ())
      length answers `shouldBe` 10

    it "demonstrate fair conjunction" $ do
      answers <- queryFile (exampleFile "fair-conjunction.ava") [qry| a(?x) |]
      answers `shouldNotSatisfy` null

    it "demonstrate fair disjunction" $ do
      answers <- queryFile (exampleFile "fair-disjunction.ava") [qry| a(?x) |]
      answers `shouldNotSatisfy` null

    -- TODO: This is slow for some reason, to the tune of 2 seconds in ghcid
    it "finds paths" $ do
      let go = queryFile (exampleFile "path.ava")

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

  describe "native predicate typeclass wizardry" $ do
    it "works on bool-valued functions on Value" $ do
      shouldSucceed $ queryRules [qry| baz(a) |]
                                 [rls| foo(a).
                                       bar(b).
                                       baz(?x) :-
                                          foo(?x),
                                          bar(?y),
                                          :prim says not=(?x, ?y). |]

    it "works on bool-valued functions on Valuable" $ do
      shouldSucceed $ queryRules [qry| baz(?x) |]
                                 [rls| foo(2).
                                       baz(?x) :-
                                         foo(?x),
                                         :prim says even(?x). |]

    it "works on Valuable-valued functions" $ do
      shouldSucceed $ queryRules [qry| palindrome(bob) |]
                                 [rls| palindrome(?x) :- :prim says rev(?x, ?x). |]

      shouldFail    $ queryRules [qry| palindrome(alice) |]
                                 [rls| palindrome(?x) :- :prim says rev(?x, ?x). |]

    it "turns lists into multiple successes" $ do
      answers <- queryRules [qry| bar(?rows) |]
                           [rls| foo("a\nb\nc").
                                 bar(?rows) :- foo(?text),
                                 :prim says lines(?text, ?rows). |]
      answers `shouldMatchList` [ Lit (Pred "bar" 1) [Val "a"]
                                , Lit (Pred "bar" 1) [Val "b"]
                                , Lit (Pred "bar" 1) [Val "c"] ]

    it "works on IO computations" $ do
      shouldSucceed $ queryRules [qry| time(?t) |]
                                 [rls| time(?t) :- :prim says cpu-time(?t). |]
      shouldFail    $ queryRules [qry| time(0)  |] -- presumably the cpu time is nonzero
                                 [rls| time(?t) :- :prim says cpu-time(?t). |]


    it "works on all the things (Int -> IO [(Int, Bool)])" $ do
      answers <- queryRules [qry| go(?b, ?x, 5) |]
                           [rls| go(?b, ?x, ?n) :- :prim says silly(?n, ?x, ?b). |]
      length answers `shouldBe` 5

