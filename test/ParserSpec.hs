{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParserSpec where

import Control.Monad
import Data.Either
import Data.Foldable
import System.Directory
import System.FilePath

import Language.Avaleryar.Parser
import Language.Avaleryar.Syntax

import Fixtures

import Test.Hspec

spec :: Spec
spec = do

  describe "quasiquoters" $ do
    it "parse queries" $ do
      [qry| quasi-query(?var, symbol, "quoted string", #f, 5) |]
         `shouldBe` Lit (Pred "quasi-query" 5) [Var "var", Val "symbol", Val "quoted string", Val (B False), Val (I 5)]

    it "parse facts" $ do
      [fct| fact(symbol) |] `shouldBe` Lit (Pred "fact" 1) [Val "symbol"]

    it "parse rules" $ do
      let ruleA = Rule (Lit (Pred "a" 1) [Var "x"]) [ARTerm (Val "qq") `Says` Lit (Pred "b" 1) [Var "x"] ]
          ruleB = Rule (Lit (Pred "b" 1) [Var "x"]) [ARTerm (Val "qq") `Says` Lit (Pred "c" 1) [Var "y"]
                                                    , ARTerm (Val "qq") `Says` Lit (Pred "a" 1) [Var "x"] ]
          ruleC = Rule (Lit (Pred "c" 1) [Var "y"]) [ARTerm (Val "t") `Says` Lit (Pred "a" 2) [Var "x", Var "y"]
                                                    , ARNative "nat" `Says` Lit (Pred "b" 1) [Var "y"] ]
      [rls| a(?x) :- b(?x). b(?x) :- c(?y), a(?x). c(?y) :- t says a(?x, ?y), :nat says b(?y). |]
         `shouldBe` [ ruleA, ruleB, ruleC ]

  describe "file parser" $ do
    it "parses examples" $ do
      files <- filter ((== ".ava") . takeExtension) <$> listDirectory exampleDir
      when (null files) $ expectationFailure ("no .ava files in example directory: " <> exampleDir)
      for_ files $ \file -> do
         parsed <- parseFile (exampleFile file) Nothing
         parsed `shouldSatisfy` isRight
