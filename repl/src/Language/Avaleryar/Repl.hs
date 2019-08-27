{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Avaleryar.Repl where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Containers.ListUtils    (nubOrd)
import           Data.Foldable
import           Data.IORef
import           Data.List                    (isPrefixOf)
import qualified Data.Map                     as Map
import           Data.String
import           Data.Text                    (unpack)
import           Options.Applicative
import           System.Console.Haskeline     (MonadException(..))
import           System.Console.Repline       as RL
import           System.IO.Unsafe
import           System.ReadEditor            (readEditorWith)
import           Text.PrettyPrint.Leijen.Text (Pretty, pretty)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Language.Avaleryar.Parser
import Language.Avaleryar.PDP           (demoConfig)
import Language.Avaleryar.PDP.Handle
import Language.Avaleryar.PrettyPrinter
import Language.Avaleryar.Syntax


-- type Repl a = HaskelineT (ReaderT (PDPHandle IO) IO) a
type Repl a = HaskelineT (ReaderT PDPHandle IO) a

cmd :: Command Repl
cmd qry = do
  let parsed = parseQuery "<interactive>" (fromString qry)
  handle <- ask
  facts  <- liftIO $ readIORef appFacts
  case parsed of
    Left err -> liftIO $ putStrLn err
    Right (Lit (Pred p _) args) -> liftIO (runQuery handle facts p args) >>= \case
      Left err -> liftIO . putStrLn $ show err
      Right answers -> if   null answers
                      then liftIO $ putStrLn "no."
                      else liftIO $ putFacts answers

banner :: Repl String
banner = pure "-? "

options :: Options Repl
options = [ ("load", load)
          , ("dump", dump)
          , ("app",  app)]

appFacts :: IORef [Fact]
appFacts = unsafePerformIO $ newIORef []
{-# NOINLINE appFacts #-}

load :: [FilePath] -> Repl ()
load paths = dontCrash $ do
  handle <- ask
  liftIO $ for_ paths $ \path -> do
    submitted <- submitFile handle path []
    case submitted of
      Left err -> putStrLn $ path ++ ": " ++ show err
      Right () -> pure ()

dump :: [String] -> Repl ()
dump assns = do
  dumped <- ask >>= liftIO . dumpDb
  let assns' | null assns = Map.keys dumped
             | otherwise  = fromString <$> assns
  for_ (nubOrd assns') $ \assn -> liftIO $ do
    traverse_ (putAssertion assn) $ Map.lookup assn dumped
    liftIO $ putStrLn ""

app :: [String] -> Repl ()
app _ = do
  currentFacts <- liftIO $ readIORef appFacts
  let header = ";; facts written below will be added to the 'application' assertion"
      body   = unlines . fmap (prettyString @(Rule TextVar) . factToRule) $ currentFacts

  newSource <- liftIO $ readEditorWith (header <> "\n\n" <> body)

  let parsed = parseFacts (fromString newSource)

  liftIO $ case parsed of
    Left err -> putStrLn err
    Right [] -> putStrLn "no facts provided, preserving current facts."
    Right fs -> do
      writeIORef appFacts fs
      putStrLn $ "loaded " <> (show $ length fs) <> " fact(s)."

commandMatcher :: (MonadIO m, MonadReader PDPHandle m) => [(String, CompletionFunc m)]
commandMatcher = [ (":load", fileCompleter)
                 , (":dump", dumpCompleter)
                 , (":app",  RL.listCompleter [])
                 ]
  where dumpCompleter ss = do -- TODO: inhibit duplicates a la zsh completion
          assns <- fmap prettyString <$> listAssertions
          RL.listCompleter assns ss

listAssertions :: (MonadIO m, MonadReader PDPHandle m) => m [Value]
listAssertions = ask >>= liftIO . fmap Map.keys . dumpDb

prettyString :: Pretty p => p -> String
prettyString = unpack . PP.displayTStrict . PP.renderCompact . pretty

byWord :: forall m. (MonadIO m, MonadReader PDPHandle m) => WordCompleter m
byWord n = do
  let names = [c | (c, _) <- commandMatcher @m]
  sysPreds <- ask >>= liftIO . dumpDb >>= pure . concat . Map.lookup "system"
  pure $ filter (isPrefixOf n) (names <> nubOrd [unpack p | (Pred p _) <- sysPreds])

ini :: Repl ()
ini = liftIO $ putStrLn "Avaleryar!"

commandChar :: Maybe Char
commandChar = Just ':'

main :: IO ()
main = do
  Right conf <- demoConfig
  handle <- newHandle conf
  flip runReaderT handle $ evalRepl banner cmd options commandChar (Prefix (wordCompleter byWord) commandMatcher) ini
