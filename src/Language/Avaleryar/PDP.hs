{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.Avaleryar.PDP where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor               (first)
import           Data.Coerce
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T
import           Data.Void                    (vacuous)
import           System.FilePath              (dropExtension)
import           Text.PrettyPrint.Leijen.Text (pretty, putDoc)

import Language.Avaleryar.ModeCheck     (modeCheck)
import Language.Avaleryar.Parser        (fct, parseFile, qry)
import Language.Avaleryar.PrettyPrinter
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax


data PDPConfig m = PDPConfig
  { systemAssertion  :: Map Pred (Lit EVar -> AvaleryarT m ()) -- ^ can't change system assertion at runtime
  , nativeAssertions :: NativeDb m -- ^ Needs to be in the reader so changes induce a new mode-check on rules
  , submitQuery      :: Maybe (Lit TextVar) -- ^ for authorizing assertion submissions
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
    deriving (Eq, Ord, Read, Show)

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
-- TODO: make a safe version
unsafeSubmitAssertion :: Monad m => Text -> [Rule TextVar] -> PDP m ()
unsafeSubmitAssertion assn rules = do
  checkRules rules
  modifyRulesDb $ insertRuleAssertion assn (compileRules rules)

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

-- | TODO: ergonomics, protect "system", etc.
unsafeSubmitFile :: MonadIO m => FilePath -> PDP m ()
unsafeSubmitFile path = do
  let munge = dropExtension
  rules <- liftIO $ parseFile path (Just munge)
  unsafeSubmitAssertion (pack $ munge path) =<< either (throwError . ParseError) (pure . coerce) rules

mkNativePred :: (ToNative a, MonadIO m) => Text -> a -> NativePred m
mkNativePred pn f = NativePred np moded
  where np (Lit _ args) = toNative f args
        modes = inferMode f
        moded = Lit (Pred pn $ length modes) (Var <$> modes)

mkNativeDb :: Monad m => Text -> [NativePred m] -> NativeDb m
mkNativeDb assn preds = NativeDb . Map.singleton assn $ Map.fromList [(p, np) | np@(NativePred _ (Lit p _)) <- preds]

demoNativeDb :: MonadIO m => NativeDb m
demoNativeDb = mkNativeDb "base" preds
  where preds = [ mkNativePred "not=" $ (/=) @Value
                , mkNativePred "even" $ even @Int
                , mkNativePred "odd"  $ odd @Int
                , mkNativePred "rev"  $ Solely . T.reverse
                , mkNativePred "cat"  $ fmap (Solely . pack) . readFile . T.unpack
                , mkNativePred "lines" $ fmap Solely . T.lines]

demo :: IO (Either PDPError (PDPConfig IO))
demo = runExceptT $ do
  let modes = fmap (fmap nativeSig) $ unNativeDb (demoNativeDb :: NativeDb IO)
  sys <- ExceptT . fmap (first ParseError . coerce) $ parseFile "system.ava" (Just dropExtension)
  ExceptT . pure . first ModeError $ modeCheck modes sys
  pure $ PDPConfig (compileRules sys) demoNativeDb (Just [qry| may(submit) |]) 50 10

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
