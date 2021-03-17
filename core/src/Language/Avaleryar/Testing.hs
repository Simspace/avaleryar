{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
-- | A small unit-testing framework exploiting 'Directive's.  This module probably belongs in a
-- different package, but is provided here because it's convenient, helpful and it provides an
-- example (though perhaps not an exemplar) of how directives can be used to provide extended
-- functionality.
--
-- NB: I'm not completely happy with how this has turned out, but I think it's a pretty decent
-- approximation of what a decent approach might look like.  I'm declaring partial victory on it for
-- the time being, but I want to enumerate several concerns here as a guide for the axe-wielding
-- successor, who will probably still be me.
--
--   * This whole file is just too shaggy.  It's possible it could be made more palatable by making
--     the idea of a "'Directive' Parser" a real thing.
--
--   * Everything here is too file-oriented.  It should be easier to use this functionality
--     programmatically, rather than by reading files off disk all the time.
--
--   * There's something irksomely untidy about simultaneously allowing rules and directives to
--     coexist in a file and yet more or less ignoring the rules that appear in "test files", so to
--     speak.
--
--   * Something isn't quite right about the whole 'TestResults' and 'putTestResults' stack.
--
--   * We don't aggregate test results---would be nice to do percentages and make it more obvious
--     when one amongst a large pile of tests has failed for better UX.
--
--   * Trying this out has convinced me that we need negative tests as well (i.e., "this query
--     fails").
--
--   * Trying this out has also convinced me that writing new databases to realize minor variants in
--     query behavior is excessively laborious.  A solution /might/ look like a collection of
--     operations on databases (union, intersection, filter, cons, whatever).  But it might be
--     something completely different that I haven't thought of yet.
--
--   * Every test framework should have @xfail@.  It should be a SMOP to add it here, but I'm still
--     ashamed to have left it off as of yet.

module Language.Avaleryar.Testing
  ( Test(..)
  , TestCase(..)
  , TestDb
  , TestResult
  , TestResults
  , parseTestFile
  , runTestFile
  , withTestHandle
  , withTestHandle_
  , prettyTestResults
  , putTestResults
  ) where

import           Data.Bool                    (bool)
import           Control.Monad                (void)
import           Data.Foldable                (for_, toList)
import qualified Data.Map                     as Map
import           Data.Text                    (Text, splitOn)
import           Data.Void                    (vacuous)
import           Text.PrettyPrint.Leijen.Text hiding (bool, (<$>))

import Language.Avaleryar.PDP        (PDPConfig(..), PDPError)
import Language.Avaleryar.PDP.Handle
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax
import Language.Avaleryar.Parser     (parseFile')


data TestCase = TestCase
  { testName    :: Text
  , testAssns   :: [(Text, Text)]
  , testQueries :: [Query]
  } deriving (Eq, Ord, Show)

data Test = Test
  { testCase :: TestCase
  , testDb   :: TestDb
  }

type TestDb = ([(Text, [Rule RawVar])], NativeDb)

parseTestAssertion :: Term a -> Maybe (Text, Text)
parseTestAssertion t = fromTerm t >>= go . splitOn "="
  where go [assn]        = Just (assn,  assn)
        go [alias, assn] = Just (alias, assn)
        go _             = Nothing

parseTestCase :: Directive -> Maybe TestCase
parseTestCase (Directive (Lit (Pred "test" _) (tn:dbs)) tqs) = do
  let testQueries = vacuous <$> tqs
  testName  <- fromTerm tn
  testAssns <- traverse parseTestAssertion dbs
  pure TestCase {..}
parseTestCase _ = Nothing

parseDb :: (Text, Text) -> Directive -> Maybe TestDb
parseDb (alias, assn) (Directive (Lit (Pred "db" _) [Val (T dbn)]) fs) | assn == dbn =
  Just ([(alias, fmap factToRule fs)], mempty)
parseDb (alias, assn) (Directive (Lit (Pred "native" _) [Val (T dbn)]) fs) | assn == dbn =
  Just (mempty, mkNativeDb alias $ factsToNative fs)
parseDb _ _ = Nothing

-- Have to group up all the facts to pass to 'compilePred' or else they won't succeed more than once
-- (i.e., @native(stuff) may(read), may(write).@ can't succeed on both @may@s without this annoying
-- grouping pass.
--
-- TODO: The fake mode might be too strong, in which case we'd need some other plan?
factsToNative :: [Fact] -> [NativePred]
factsToNative fs = [NativePred (\l -> void $ compilePred rs l) (modeFor p) | (p, rs) <- Map.toList preds]
  where preds = Map.fromListWith (<>) [(p, [factToRule f]) | f@(Lit p _) <- fs]
        modeFor p@(Pred _ n) = Lit p (replicate n (Var outMode))

dbForTestCase :: [Directive] -> TestCase -> TestDb
dbForTestCase dirs TestCase {..} = foldMap go testAssns
  where go p = maybe mempty id $ foldMap (parseDb p) dirs

-- | Find assertions used by the 'Test' that aren't available in its 'TestDb'.
--
-- FIXME: The implementation is pretty terrible and non-intuitive---we have to invert the aliasing
-- we did in 'parseDb' to find the names of the missing assertions.  I think it may still be a tad
-- busted, but I'm moving on for now.
missingAssertions :: Test -> [Text]
missingAssertions (Test tc (rs, ndb)) = unalias . filter (`notElem` assns) $ (fst <$> testAssns tc)
  where assns   = fmap fst rs <> (Map.keys . unNativeDb $ ndb)
        unalias = foldMap (toList . flip lookup (testAssns tc))

-- TODO: Pretty output for test results.
data TestResult = Pass | Fail | Error PDPError
  deriving (Eq, Ord, Show)

data TestError = MissingAssertions [Text]
  deriving (Eq, Ord, Show)

type TestResults = Either TestError [(Query, TestResult)]

instance Pretty TestResult where
  pretty Pass      = "ok"
  pretty Fail      = "fail"
  pretty (Error e) = pretty $ show e -- TODO: Suck Less

instance Pretty TestError where
  pretty (MissingAssertions as) = "assertions missing: " <> (hsep . punctuate "," $ fmap pretty as)

prettyTestResults :: Text -> TestResults -> Doc
prettyTestResults tn rs = pretty tn <> nest 2 prs
  where prs       = either pretty ((line<>) . vsep . fmap pr) $ rs
        pr (q, r) = fillBreak 30 (pretty q <> colon) <+> pretty r

putTestResults :: Text -> TestResults -> IO ()
putTestResults tn rs = putDoc $ prettyTestResults tn rs <> line

runTest :: PDPHandle -> Test -> IO TestResults
runTest hdl t = go (missingAssertions t)
  where app   = appAssertion t
        go [] = fmap Right . traverse (runTestQuery' hdl app) . testQueries . testCase $ t
        go as = pure . Left . MissingAssertions $ as

runTestQuery :: PDPHandle -> [Fact] -> Query -> IO TestResult
runTestQuery hdl app (Lit (Pred p _) as) = resultify <$> checkQuery hdl app p as
  where resultify = either Error (bool Fail Pass)

runTestQuery' :: PDPHandle -> [Fact] -> Query -> IO (Query, TestResult)
runTestQuery' hdl app q = (q,) <$> runTestQuery hdl app q

-- | Sneakily smash the given DB of native assertions over the entries in the 'PDPConfig'.  This
-- leans on the whole left-biased map-union thing to let the tests using this override whatever was
-- there before, when it can.
insinuateNativeDb :: NativeDb -> PDPConfig -> PDPConfig
insinuateNativeDb ndb conf@PDPConfig {..} = conf {nativeAssertions = ndb `go` nativeAssertions}
  where go (NativeDb n) (NativeDb nas) = NativeDb $ Map.unionWith (<>) n nas

-- Remember the callback is for adding more assertions.
withTestHandle :: PDPConfig -> (PDPHandle -> IO a) -> Test -> IO (TestResults, a)
withTestHandle conf k t@(Test _ (assns, ndb)) = do
  hdl <- newHandle $ insinuateNativeDb ndb conf
  for_ assns $ uncurry (unsafeSubmitAssertion hdl)
  a <- k hdl
  results <- runTest hdl t
  pure (results, a)

withTestHandle_ :: PDPConfig -> (PDPHandle -> IO ()) -> Test -> IO TestResults
withTestHandle_ p k t = fst <$> withTestHandle p k t

extractTests :: [Directive] -> [Test]
extractTests dirs = go <$> cases
  where cases = foldMap (toList . parseTestCase) dirs
        go tc = Test tc $ dbForTestCase dirs tc

-- | Turn a 'Rule' that's really a fact into a 'Fact' in fact.  Hideously unsafe if you don't
-- already know for sure it'll succeed.  This function really shouldn't escape this module.
ruleToFact :: Rule v -> Fact
ruleToFact (Rule hd []) = fmap go hd
  where go _ = error "variable in rule coerced to fact---absurdity!"
ruleToFact _ = error "body lits in rule coerced to fact---insanity!"

-- | Construct an @application@ assertion to provide along with the test queries by picking the
-- first assertion in the 'testDb' with the name (or, well alias) @application@.
appAssertion :: Test -> [Fact]
appAssertion = fmap ruleToFact . concat . lookup "application" . fst . testDb

parseTestFile :: FilePath -> IO (Either String [Test])
parseTestFile fp = fmap (extractTests . fst) <$> parseFile' fp

runTestFile :: PDPConfig -> (PDPHandle -> IO ()) -> FilePath -> IO (Either String [(Text, TestResults)])
runTestFile conf k tf = do
  let gatherResults t@Test {..} = (testName testCase,) <$> withTestHandle_ conf k t
  parsed <- parseTestFile tf
  case parsed of
    Left err -> pure (Left err)
    Right ts -> Right <$> traverse gatherResults ts
