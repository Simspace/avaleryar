{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module Language.Avaleryar.PDP where

import           Control.Exception            (Exception)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor               (first)
import           Data.Coerce
import           Data.List                    (stripPrefix)
import           Data.Map                     (Map)
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable)
import           System.FilePath              (stripExtension)
import           Text.PrettyPrint.Leijen.Text (Pretty(..), putDoc, squotes)

import Language.Avaleryar.ModeCheck     (ModeError, modeCheck)
import Language.Avaleryar.Parser        (parseFile, parseText, qry)
import Language.Avaleryar.PrettyPrinter ()
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax


data PDPConfig = PDPConfig
  { systemAssertion  :: Map Pred (Lit EVar -> Avaleryar ()) -- ^ can't change system assertion at runtime
  , nativeAssertions :: NativeDb   -- ^ Needs to be in the reader so changes induce a new mode-check on rules
  , submitQuery      :: Maybe Query -- ^ for authorizing assertion submissions
  , maxDepth         :: Int
  , maxAnswers       :: Int
  }

newtype PDP a = PDP { unPDP :: ReaderT PDPConfig (ExceptT PDPError (StateT RulesDb IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError PDPError)

data PDPError
  = ModeError ModeError
  | VarInQueryResults TextVar
  | ParseError String
  | SubmitError SubmitError
    deriving (Eq, Ord, Read, Show, Typeable)

instance Exception PDPError

instance Pretty PDPError where
  pretty (ModeError e)         = pretty e
  pretty (VarInQueryResults v) = "got variable " <> squotes (pretty v) <> " in query results"
  pretty (ParseError e)        = pretty e
  pretty (SubmitError e)       = pretty e

data SubmitError
  = SubmissionDisabled
  | SubmissionDenied
    deriving (Eq, Ord, Read, Show)

instance Pretty SubmitError where
  pretty SubmissionDisabled = "submission disabled"
  pretty SubmissionDenied   = "submission denied"

runPDP :: PDP a -> PDPConfig -> IO (Either PDPError a)
runPDP (PDP ma) = flip evalStateT mempty . runExceptT . runReaderT ma

runPDP' :: PDP a -> PDPConfig -> IO a
runPDP' pdp conf = runPDP pdp conf >>= either (error . show) pure

runAva :: Avaleryar a -> PDP (AvaResults a)
runAva = runAvaWith id

-- | Run an 'AvaleryarT' computation inside a 'PDP', configured according to the latter's
-- 'PDPConfig'.  The caller is given an opportunity to muck with the 'RulesDb' with which the
-- subcomputation is run.  This is used by 'runQuery' to add the @application@ assertion from the
-- query just before executation.
--
-- NB: The system assertion from the config is added to the the rule database after the caller's
-- mucking function has done its business to ensure that the caller can't sneakily override the
-- @system@ assertion with their own.
runAvaWith :: (RulesDb -> RulesDb) -> Avaleryar a -> PDP (AvaResults a)
runAvaWith f ma = avaResults <$> runDetailedWith f ma

runDetailedWith :: (RulesDb -> RulesDb) -> Avaleryar a -> PDP (DetailedResults a)
runDetailedWith f ma = do
  PDPConfig {..} <- askConfig
  -- do 'f' *before* inserting the system assertion, to make sure the caller can't override it!
  rdb            <- insertRuleAssertion "system" systemAssertion . f <$> getRulesDb
  liftIO $ runAvalaryarT' maxDepth maxAnswers (Db (f rdb) nativeAssertions) ma
  -- is this exactly what I just said not to do  ^ ?

checkRules :: [Rule RawVar] -> PDP ()
checkRules rules = do
  nm <- asksConfig (fmap (fmap nativeSig) . unNativeDb . nativeAssertions) -- TODO: Suck less
  either (throwError . ModeError) pure $ modeCheck nm rules

checkSubmit :: [Fact] -> PDP ()
checkSubmit facts = asksConfig submitQuery >>= \case
    Nothing -> throwError $ SubmitError SubmissionDisabled
    Just q -> do
      answers <- runQuery' facts q
      when (null answers) $ throwError (SubmitError SubmissionDenied)

submitAssertion :: Text -> [Rule RawVar] -> [Fact] -> PDP ()
submitAssertion assn rules facts = checkSubmit facts >> unsafeSubmitAssertion assn rules

submitText :: Text -> Text -> [Fact] -> PDP ()
submitText assn text facts = checkSubmit facts >> unsafeSubmitText assn text

submitFile :: Maybe String -> FilePath -> [Fact] -> PDP ()
submitFile assn path facts = checkSubmit facts >> unsafeSubmitFile assn path

-- | unsafe because there's no authz on the submission
unsafeSubmitAssertion :: Text -> [Rule RawVar] -> PDP ()
unsafeSubmitAssertion assn rules = do
  checkRules rules
  modifyRulesDb $ insertRuleAssertion assn (compileRules assn $ fmap (fmap unRawVar) rules)


-- | TODO: ergonomics, protect "system", etc.
unsafeSubmitFile :: Maybe String -> FilePath -> PDP ()
unsafeSubmitFile assn path = do
  rules <- liftIO $ parseFile path
  unsafeSubmitAssertion (pack $ maybe (stripDotAva path) id assn) =<< either (throwError . ParseError) (pure . coerce) rules

unsafeSubmitText :: Text -> Text -> PDP ()
unsafeSubmitText assn text = unsafeSubmitAssertion assn =<< either (throwError . ParseError) (pure . coerce) rules
  where rules = parseText assn text

retractAssertion :: Text -> PDP ()
retractAssertion = modifyRulesDb . retractRuleAssertion

runDetailedQuery :: [Fact] -> Text -> [Term TextVar] -> PDP DetailedQueryResults
runDetailedQuery facts p args  = do
  answers <- runDetailedWith (insertApplicationAssertion facts) $ compileQuery "system" p args
  flip traverse answers $ \l -> do
     traverse (throwError . VarInQueryResults . unEVar) l

runQuery :: [Fact] -> Text -> [Term TextVar] -> PDP QueryResults
runQuery facts p args  = do
  answers <- runAvaWith (insertApplicationAssertion facts) $ compileQuery "system" p args
  flip traverse answers $ \l -> do
     traverse (throwError . VarInQueryResults . unEVar) l

runQuery' :: [Fact] -> Query -> PDP QueryResults
runQuery' facts (Lit (Pred p _) as) = runQuery facts p as

queryPretty :: [Fact] -> Text -> [Term TextVar] -> PDP ()
queryPretty facts p args = do
  answers <- runQuery facts p args
  liftIO $ mapM_ (putDoc . pretty . factToRule @TextVar) answers

testQuery :: [Fact] -> Query -> PDP ()
testQuery facts (Lit (Pred p _) as) = queryPretty facts p as

-- | Insert an @application@ assertion into a 'RulesDb' providing the given facts.
insertApplicationAssertion :: [Fact] -> RulesDb -> RulesDb
insertApplicationAssertion = insertRuleAssertion "application" . compileRules "application" . fmap factToRule

nativeModes :: NativeDb -> Map Text (Map Pred ModedLit)
nativeModes = fmap (fmap nativeSig) . unNativeDb

stripDotAva :: FilePath -> FilePath
stripDotAva path = maybe path id $ stripExtension "ava" path

stripPathPrefix :: String -> FilePath -> FilePath
stripPathPrefix pfx path = maybe path id $ stripPrefix pfx path

-- NB: The given file is parsed as the @system@ assertion regardless of its filename, which is
-- almost guaranteed to be what you want.
pdpConfig :: NativeDb -> FilePath -> IO (Either PDPError PDPConfig)
pdpConfig db fp = runExceptT $ do
  sys <- ExceptT . liftIO . fmap (first ParseError . coerce) $ parseFile fp
  ExceptT . pure . first ModeError $ modeCheck (nativeModes db) sys
  pure $ PDPConfig (compileRules "system" $ fmap (fmap unRawVar) sys) db Nothing 50 10

pdpConfigText :: NativeDb -> Text -> Either PDPError PDPConfig
pdpConfigText db text = do
  sys <- first ParseError . coerce $ parseText "system" text
  first ModeError $ modeCheck (nativeModes db) sys
  pure $ PDPConfig (compileRules "system" $ fmap (fmap unRawVar) sys) db Nothing 50 10

pdpConfigRules :: NativeDb -> [Rule RawVar] -> Either PDPError PDPConfig
pdpConfigRules db sys = do
  first ModeError $ modeCheck (nativeModes db) sys
  pure $ PDPConfig (compileRules "system" $ fmap (fmap unRawVar) sys) db Nothing 50 10


demoNativeDb :: NativeDb
demoNativeDb = mkNativeDb "base" preds
  where preds = [ mkNativePred "not=" $ (/=) @Value
                , mkNativePred "even" $ even @Int
                , mkNativePred "odd"  $ odd @Int
                , mkNativePred "rev"  $ Solely . T.reverse
                , mkNativePred "cat"  $ fmap (Solely . pack) . readFile . T.unpack
                , mkNativePred "lines" $ fmap Solely . T.lines]

demoConfig :: IO (Either PDPError PDPConfig)
demoConfig = fmap addSubmit <$> pdpConfig demoNativeDb "system.ava"
  where addSubmit conf = conf { submitQuery = Just [qry| may(submit) |]}

-- Everyone: Alec, why not just use lenses?
-- Me: ... what's that over there!? ... ::smokebomb::

asksConfig :: (PDPConfig -> a) -> PDP a
asksConfig f = PDP $ asks f

askConfig :: PDP PDPConfig
askConfig = asksConfig id

getsRulesDb :: (RulesDb -> a) -> PDP a
getsRulesDb f = PDP $ gets f

getRulesDb :: PDP RulesDb
getRulesDb = getsRulesDb id

modifyRulesDb :: (RulesDb -> RulesDb) -> PDP ()
modifyRulesDb f = PDP $ modify f

putRulesDb :: RulesDb -> PDP ()
putRulesDb ndb = modifyRulesDb (const ndb)

withMaxDepth :: Int -> PDP a -> PDP a
withMaxDepth n = PDP . local go . unPDP
  where go conf = conf {maxDepth = n}

withMaxAnswers :: Int -> PDP a -> PDP a
withMaxAnswers n = PDP . local go . unPDP
  where go conf = conf {maxAnswers = n}
