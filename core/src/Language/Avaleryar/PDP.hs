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
import           Text.PrettyPrint.Leijen.Text (pretty, putDoc)

import Language.Avaleryar.ModeCheck     (modeCheck)
import Language.Avaleryar.Parser        (parseFile, qry)
import Language.Avaleryar.PrettyPrinter ()
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax


-- maybe do this
class Monad m => MonadPDP m where
  cunsafeSubmitAssertion :: Text -> [Rule TextVar] -> m (Either PDPError ())
  cretractAssertion      :: Text -> m ()
  crunQuery              :: [Fact] -> Text -> [Term TextVar] -> m [Fact]
  ccheckQuery            :: [Fact] -> Text -> [Term TextVar] -> m Bool
  ccheckQuery fs assn q  = null <$> crunQuery fs assn q

data PDPConfig m = PDPConfig
  { systemAssertion  :: Map Pred (Lit EVar -> AvaleryarT m ()) -- ^ can't change system assertion at runtime
  , nativeAssertions :: NativeDb m -- ^ Needs to be in the reader so changes induce a new mode-check on rules
  , submitQuery      :: Maybe (Lit TextVar) -- ^ for authorizing assertion submissions
  , mungeAssertion   :: FilePath -> FilePath -- ^ for turning filenames into assertion names
  , maxDepth         :: Int
  , maxAnswers       :: Int
  }

newtype PDP m a = PDP { unPDP :: ReaderT (PDPConfig m) (ExceptT PDPError (StateT (RulesDb m) m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError PDPError)

instance MonadTrans PDP where
  lift = PDP . lift . lift . lift

data PDPError
  = ModeError Text
  | VarInQueryResults TextVar
  | ParseError String
  | SubmitError SubmitError
    deriving (Eq, Ord, Read, Show, Typeable)

instance Exception PDPError

data SubmitError
  = SubmissionDisabled
  | SubmissionDenied
    deriving (Eq, Ord, Read, Show)

runPDP :: MonadIO m => PDP m a -> PDPConfig m -> m (Either PDPError a)
runPDP (PDP ma) = flip evalStateT mempty . runExceptT . runReaderT ma

runPDP' :: PDP IO a -> PDPConfig IO -> IO a
runPDP' pdp conf = runPDP pdp conf >>= either (error . show) pure

runAva :: Monad m => AvaleryarT m a -> PDP m [a]
runAva = runAvaWith id

-- | Run an 'AvaleryarT' computation inside a 'PDP', configured according to the latter's
-- 'PDPConfig'.  The caller is given an opportunity to muck with the 'RulesDb' with which the
-- subcomputation is run.  This is used by 'runQuery' to add the @application@ assertion from the
-- query just before executation.
--
-- NB: The system assertion from the config is added to the the rule database after the caller's
-- mucking function has done its business to ensure that the caller can't sneakily override the
-- @system@ assertion with their own.
runAvaWith :: Monad m => (RulesDb m -> RulesDb m) -> AvaleryarT m a -> PDP m [a]
runAvaWith f ma = do
  PDPConfig {..} <- askConfig
  -- do 'f' *before* inserting the system assertion, to make sure the caller can't override it!
  rdb            <- insertRuleAssertion "system" systemAssertion . f <$> getRulesDb
  lift $ runAvalaryarT maxDepth maxAnswers (Db (f rdb) nativeAssertions) ma
                                             -- ^ is this exactly what I just said not to do?

checkRules :: Monad m => [Rule TextVar] -> PDP m ()
checkRules rules = do
  nm <- asksConfig (fmap (fmap nativeSig) . unNativeDb . nativeAssertions) -- TODO: Suck less
  either (throwError . ModeError) pure $ modeCheck nm rules

checkSubmit :: MonadIO m => [Fact] -> PDP m ()
checkSubmit facts = asksConfig submitQuery >>= \case
    Nothing -> throwError $ SubmitError SubmissionDisabled
    Just q -> do
      answers <- runQuery' facts q
      when (null answers) $ throwError (SubmitError SubmissionDenied)

submitAssertion :: MonadIO m => Text -> [Rule TextVar] -> [Fact] -> PDP m ()
submitAssertion assn rules facts = checkSubmit facts >> unsafeSubmitAssertion assn rules

submitFile :: MonadIO m => FilePath -> [Fact] -> PDP m ()
submitFile path facts = checkSubmit facts >> unsafeSubmitFile path

-- | unsafe because there's no authz on the submission
unsafeSubmitAssertion :: Monad m => Text -> [Rule TextVar] -> PDP m ()
unsafeSubmitAssertion assn rules = do
  checkRules rules
  modifyRulesDb $ insertRuleAssertion assn (compileRules rules)


-- | TODO: ergonomics, protect "system", etc.
unsafeSubmitFile :: MonadIO m => FilePath -> PDP m ()
unsafeSubmitFile path = do
  munge <- asksConfig mungeAssertion
  rules <- liftIO $ parseFile path (Just munge)
  unsafeSubmitAssertion (pack $ munge path) =<< either (throwError . ParseError) (pure . coerce) rules

retractAssertion :: Monad m => Text -> PDP m ()
retractAssertion = modifyRulesDb . retractRuleAssertion

runQuery :: Monad m => [Fact] -> Text -> [Term TextVar] -> PDP m [Fact]
runQuery facts p args  = do
  answers <- runAvaWith (insertApplicationAssertion facts) $ query "system" p args
  flip traverse answers $ \lit -> do
     traverse (throwError . VarInQueryResults . snd) lit

runQuery' :: MonadIO m => [Fact] -> Lit TextVar -> PDP m [Fact]
runQuery' facts (Lit (Pred p _) as) = runQuery facts p as

queryPretty :: MonadIO m => [Fact] -> Text -> [Term TextVar] -> PDP m ()
queryPretty facts p args = do
  answers <- runQuery facts p args
  liftIO $ mapM_ (putDoc . pretty . factToRule @TextVar) answers

testQuery :: MonadIO m => [Fact] -> Lit TextVar -> PDP m ()
testQuery facts (Lit (Pred p _) as) = queryPretty facts p as

-- | Insert an @application@ assertion into a 'RulesDb' providing the given facts.
insertApplicationAssertion :: Monad m => [Fact] -> RulesDb m -> RulesDb m
insertApplicationAssertion = insertRuleAssertion "application" . compileRules . fmap factToRule

nativeModes :: NativeDb m -> Map Text (Map Pred ModedLit)
nativeModes = fmap (fmap nativeSig) . unNativeDb

stripDotAva :: FilePath -> FilePath
stripDotAva path = maybe path id $ stripExtension "ava" path

stripPathPrefix :: String -> FilePath -> FilePath
stripPathPrefix pfx path = maybe path id $ stripPrefix pfx path

-- NB: The given file is parsed as the @system@ assertion regardless of its filename, which is
-- almost guaranteed to be what you want.
pdpConfig :: MonadIO m => (FilePath -> FilePath) -> NativeDb m -> FilePath -> m (Either PDPError (PDPConfig m))
pdpConfig munge db fp = runExceptT $ do
  sys <- ExceptT . liftIO . fmap (first ParseError . coerce) $ parseFile fp (Just $ const "system")
  ExceptT . pure . first ModeError $ modeCheck (nativeModes db) sys
  pure $ PDPConfig (compileRules sys) db Nothing (munge . stripDotAva) 50 10

demoNativeDb :: MonadIO m => NativeDb m
demoNativeDb = mkNativeDb "base" preds
  where preds = [ mkNativePred "not=" $ (/=) @Value
                , mkNativePred "even" $ even @Int
                , mkNativePred "odd"  $ odd @Int
                , mkNativePred "rev"  $ Solely . T.reverse
                , mkNativePred "cat"  $ fmap (Solely . pack) . readFile . T.unpack
                , mkNativePred "lines" $ fmap Solely . T.lines]

demoConfig :: IO (Either PDPError (PDPConfig IO))
demoConfig = fmap addSubmit <$> pdpConfig id demoNativeDb "system.ava"
  where addSubmit conf = conf { submitQuery = Just [qry| may(submit) |]}

-- Everyone: Alec, why not just use lenses?
-- Me: ... what's that over there!? ... ::smokebomb::

asksConfig :: Monad m => (PDPConfig m -> a) -> PDP m a
asksConfig f = PDP $ asks f

askConfig :: Monad m => PDP m (PDPConfig m)
askConfig = asksConfig id

getsRulesDb :: Monad m => (RulesDb m -> a) -> PDP m a
getsRulesDb f = PDP $ gets f

getRulesDb :: Monad m => PDP m (RulesDb m)
getRulesDb = getsRulesDb id

modifyRulesDb :: Monad m => (RulesDb m -> RulesDb m) -> PDP m ()
modifyRulesDb f = PDP $ modify f

putRulesDb :: Monad m => RulesDb m -> PDP m ()
putRulesDb ndb = modifyRulesDb (const ndb)
