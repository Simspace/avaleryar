{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.Avaleryar.PDP.Handle where

import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)

import           Language.Avaleryar.PDP           (PDP(..), PDPConfig(..), PDPError)
import qualified Language.Avaleryar.PDP           as PDP
import           Language.Avaleryar.PrettyPrinter (putRulesDb)
import           Language.Avaleryar.Semantics
import           Language.Avaleryar.Syntax

data PDPHandle = PDPHandle (PDPConfig IO) (MVar (RulesDb IO))

newHandle :: PDPConfig IO -> IO PDPHandle
newHandle c = PDPHandle c <$> newMVar mempty

withPDPHandle :: PDPHandle -> PDP IO a -> IO (Either PDPError a)
withPDPHandle (PDPHandle c mv) (PDP ma) = do
  rdb <- liftIO $ readMVar mv
  flip evalStateT rdb . runExceptT $ runReaderT ma c

modifyWithPDPHandle :: PDPHandle -> PDP IO a -> IO (Either PDPError a)
modifyWithPDPHandle (PDPHandle c mv) (PDP ma) = liftIO . modifyMVar mv $ \rdb -> do
  (a, rdb') <- flip runStateT rdb . runExceptT $ runReaderT ma c
  pure (rdb', a)

submitAssertion :: PDPHandle -> Text -> [Rule TextVar] -> [Fact] -> IO (Either PDPError ())
submitAssertion h assn rules facts = modifyWithPDPHandle h $ PDP.submitAssertion assn rules facts

retractAssertion :: PDPHandle -> Text -> IO (Either PDPError ())
retractAssertion h = modifyWithPDPHandle h . PDP.retractAssertion

submitFile :: PDPHandle -> FilePath -> [Fact] -> IO (Either PDPError ())
submitFile h path facts = modifyWithPDPHandle h $ PDP.submitFile path facts

runQuery :: PDPHandle -> [Fact] -> Text -> [Term TextVar] -> IO (Either PDPError [Fact])
runQuery h facts p args = withPDPHandle h $ PDP.runQuery facts p args

checkQuery :: PDPHandle -> [Fact] -> Text -> [Term TextVar] -> IO (Either PDPError Bool)
checkQuery h facts p args = runQuery h facts p args >>= pure . fmap (not . null)

dumpDb :: PDPHandle -> IO (Map Value [Pred])
dumpDb (PDPHandle PDPConfig {..} mv) = do
  RulesDb rdb <- insertRuleAssertion "system" systemAssertion <$> readMVar mv
  pure $ fmap Map.keys rdb
