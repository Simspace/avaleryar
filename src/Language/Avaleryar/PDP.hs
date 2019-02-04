{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.Avaleryar.PDP where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor       (first)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)

import Language.Avaleryar.ModeCheck (ModedLit, modeCheck)
import Language.Avaleryar.Semantics
import Language.Avaleryar.Syntax

data PDPConfig = PDPConfig
  { systemAssertion :: [Rule TextVar]
  , nativeModes     :: Map Text (Map Pred ModedLit)
  , maxDepth        :: Int
  , maxAnswers      :: Int
  } deriving (Eq, Ord, Read, Show)

newtype PDP m a = PDP { unPDP :: ReaderT PDPConfig (ExceptT PDPError (StateT (Db m) m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError PDPError)

instance MonadTrans PDP where
  lift = PDP . lift . lift . lift

data PDPError = ModeError Text
              | VarInQueryResults TextVar
  deriving (Eq, Ord, Read, Show)

runAva :: Monad m => AvaleryarT m a -> PDP m [a]
runAva ma = do
  PDPConfig {..} <- askConfig
  rtdb           <- getDb
  lift $ runAvalaryarT maxDepth maxAnswers rtdb ma


checkRules :: Monad m => [Rule TextVar] -> PDP m ()
checkRules rules = do
  nm <- asksConfig nativeModes
  either (throwError . ModeError) pure $ modeCheck nm rules

-- | unsafe because there's no authz on the submission
-- TODO: make a safe version
unsafeSubmitAssertion :: Monad m => Text -> [Rule TextVar] -> PDP m ()
unsafeSubmitAssertion assn rules = do
  checkRules rules
  modifyDb $ insertRuleAssertion assn (compileRules rules)

runQuery :: Monad m => Text -> [Term TextVar] -> PDP m [Fact]
runQuery p args = do
  answers <- runAva $ query "system" p args
  flip traverse answers $ \lit -> do
     traverse (throwError . VarInQueryResults . snd) lit






asksConfig :: Monad m => (PDPConfig -> a) -> PDP m a
asksConfig f = PDP $ asks f

askConfig :: Monad m => PDP m PDPConfig
askConfig = asksConfig id

getsDb :: Monad m => (Db m -> a) -> PDP m a
getsDb f = PDP $ gets f

getDb :: Monad m => PDP m (Db m)
getDb = getsDb id

modifyDb :: Monad m => (Db m -> Db m) -> PDP m ()
modifyDb f = PDP $ modify f

putDb :: Monad m => Db m -> PDP m ()
putDb ndb = modifyDb (const ndb)

getRulesDb :: Monad m => PDP m (RulesDb m)
getRulesDb = getsDb rulesDb

modifyRulesDb :: Monad m => (RulesDb m -> RulesDb m) -> PDP m ()
modifyRulesDb f = modifyDb go
  where go db = db { rulesDb = f (rulesDb db) }

putRulesDb :: Monad m => RulesDb m -> PDP m ()
putRulesDb rdb = modifyRulesDb (const rdb)
