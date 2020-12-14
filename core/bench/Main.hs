{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Criterion.Main
import Data.Bool
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text (displayTStrict, renderPretty, vcat, Pretty(..))

import Language.Avaleryar
import Language.Avaleryar.PDP
import Language.Avaleryar.Semantics hiding (env)
import Language.Avaleryar.Syntax


main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks = [ bgroup "clique" [clique n | n <- [5, 10, 25, 40]]
             , bgroup "line"   [line   n | n <- [5, 10, 25, 40]]
             , bgroup "loop"   [loop   n | n <- [50, 100, 500, 1000]]
             , bgroup "tight"  [tight  n | n <- [50, 100, 500, 1000]]
             , bgroup "parse"  [parse  n | n <- [50, 100, 500, 1000]]
             ]

-- | Generate a complete graph of size @n@ and try to find a path from the first node to the last.
-- It uses the potentially-really-bad transitive closure (see @path@ below), so if this is measuring
-- anything, it's probably about interleaving.  The @application@ assertion looks like this:
--
-- @
--   edge(1, 1).
--   edge(1, 2).
--   edge(1, 3).
--   edge(2, 1).
--   edge(2, 2).
--   edge(2, 3).
--   edge(3, 1).
--   edge(3, 2).
--   edge(3, 3).
--   etc...
-- @
clique :: Int -> Benchmark
clique n = do
  let path = [rls| path(?x, ?y) :- application says edge(?x, ?y).
                   path(?x, ?z) :- path(?x, ?y), path(?y, ?z). |]
      edges = [lit "edge" [val x, val y]
                 | x <- [1..n]
                 , y <- [1..n]]
      go hdl = bench (show n) $ whnfIO $ do
        res <- checkQuery hdl edges "path" [val (1 :: Int), val n]
        either (error . show) (bool (error "no path found") (pure True)) res

  flip env go $ do
    let Right cfg = pdpConfigRules mempty path
    newHandle cfg {maxDepth = 4000}

-- | Generates a linear sequence of @n@ facts and tries to find a path from the first node to the
-- last.  It uses a cons-list shaped @path@ rule.  The @application@ assertion looks like this:
--
-- @
--   edge(1, 2).
--   edge(2, 3).
--   edge(3, 4).
--   etc...
-- @
line :: Int -> Benchmark
line n = do
  let path = [rls| path(?x, ?y) :- application says edge(?x, ?y).
                   path(?x, ?z) :- application says edge(?x, ?y), path(?y, ?z). |]
      edges = [lit "edge" [val x, val y]
                 | (x, y) <- zip [1..pred n] [2..n]]
      go hdl = bench (show n) $ whnfIO $ do
        res <- checkQuery hdl edges "path" [val (1 :: Int), val n]
        either (error . show) (bool (error "no path found") (pure True)) res

  flip env go $ do
    let Right cfg = pdpConfigRules mempty path
    newHandle cfg {maxDepth = 4000}

-- | Generates a loop of five rules (@e@ implies @d@ implies @c@ implies @b@ implies @a@ implies
-- @e@), each of arity 4 (so that the benchmark exercises the unification code), and then runs a
-- query with @n@ fuel.
loop :: Int -> Benchmark
loop n = do
  let rules = [rls| a(?x, ?y, ?z, ?w) :- b(?x, ?y, ?z, ?w).
                    b(?x, ?y, ?z, ?w) :- c(?x, ?y, ?z, ?w).
                    c(?x, ?y, ?z, ?w) :- d(?x, ?y, ?z, ?w).
                    d(?x, ?y, ?z, ?w) :- e(?x, ?y, ?z, ?w).
                    e(?x, ?y, ?z, ?w) :- a(?x, ?y, ?z, ?w). |]
      go hdl = bench (show n) $ whnfIO $ do
        res <- checkQuery hdl [] "a" [val (i :: Int) | i <- [1..4]]
        either (error . show) (bool (pure True) (error "loop shouldn't succeed")) res

  flip env go $ do
    let Right cfg = pdpConfigRules mempty rules
    newHandle cfg {maxDepth = n}

-- | As 'loop', but with arity 0 rules.  This should be a better measure of the overhead of the
-- underlying monad, at least insofar as there won't be unification/substitution overhead.  Expect
-- this to be faster than 'loop'.
tight :: Int -> Benchmark
tight n = do
  let rules = [rls| a :- b. b :- c. c :- d. d :- e. e :- a. |]
      go hdl = bench (show n) $ whnfIO $ do
        res <- checkQuery hdl [] "a" []
        either (error . show) (bool (pure True) (error "tight shouldn't succeed")) res

  flip env go $ do
    let Right cfg = pdpConfigRules mempty rules
    newHandle cfg {maxDepth = n}

-- | Generates @n@ rules, pretty prints them, then times how long it takes to parse them.  The text
-- look like this:
--
-- @
-- rule-1(?x, ?y, ?z, ?w) :-
--   application says rule-1-body(?x, ?y, ?z, ?w),
--   application says rule-1-body(?x, ?y, ?z, ?w),
--   application says rule-1-body(?x, ?y, ?z, ?w),
--   application says rule-1-body(?x, ?y, ?z, ?w),
--   application says rule-1-body(?x, ?y, ?z, ?w).
--
-- rule-2(?x, ?y, ?z, ?w) :-
--   application says rule-2-body(?x, ?y, ?z, ?w),
--   application says rule-2-body(?x, ?y, ?z, ?w),
--   application says rule-2-body(?x, ?y, ?z, ?w),
--   application says rule-2-body(?x, ?y, ?z, ?w),
--   application says rule-2-body(?x, ?y, ?z, ?w).
--
--   etc...
-- @
parse :: Int -> Benchmark
parse n = go txt
  where rule x = Rule (lit (rn x) vars) [Says (ARTerm (val $ T "application")) (lit (rn x <> "-body") vars) | _ <- [1..5]]
        rn x = pack ("rule-" <> show x)
        vars = Var <$> [pack "x", "y", "z", "w"]
        rs = [rule x | x <- [1..n]]
        !txt = displayTStrict . renderPretty 1.0 50 . vcat . fmap pretty $ rs
        go = bench (show n) . nf (parseText "system")
