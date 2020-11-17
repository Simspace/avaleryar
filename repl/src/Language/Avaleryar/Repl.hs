{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Avaleryar.Repl where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Containers.ListUtils    (nubOrd)
import           Data.Foldable
import           Data.IORef
import           Data.List                    (isPrefixOf)
import qualified Data.Map                     as Map
import           Data.String
import           Data.Text                    (unpack, Text)
import           Options.Applicative          as Opts
import           System.Console.Repline       as RL hiding (banner, options)
import           System.IO.Unsafe
import           System.ReadEditor            (readEditorWith)
import           Text.PrettyPrint.Leijen.Text (Pretty, pretty)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Language.Avaleryar.Parser
import Language.Avaleryar.PDP           (PDPConfig, demoNativeDb, pdpConfig)
import Language.Avaleryar.PDP.Handle
import Language.Avaleryar.PrettyPrinter
import Language.Avaleryar.Semantics     (DetailedResults(..), DetailedQueryResults)
import Language.Avaleryar.Syntax
import Language.Avaleryar.Testing       (runTestFile, putTestResults, TestResults)

-- | Spin up a repl using the given 'PDPConfig', configuring its underlying 'PDPHandle' with a
-- callback.
replWithHandle :: PDPConfig IO -> (PDPHandle -> IO ()) -> IO ()
replWithHandle conf k = do
  let complete = Prefix (wordCompleter byWord) commandMatcher
  handle <- newHandle conf
  k handle
  runReaderT (evalRepl banner cmd options commandChar complete ini) handle

-- | As 'replWithHandle', doing no additional configuration.
repl :: PDPConfig IO -> IO ()
repl conf = replWithHandle conf mempty

main :: IO ()
main = do
  Args {..}  <- execParser (info parseArgs mempty)
  Right conf <- pdpConfig demoNativeDb systemAssn
  let loadAssns h = for_ otherAssns $ either (error . show) pure <=< unsafeSubmitFile h Nothing
      displayResults = traverse_ $ either putStrLn (traverse_ $ uncurry putTestResults)
  if   null testFiles
  then replWithHandle conf loadAssns
  else runTestFiles conf loadAssns testFiles >>= displayResults

runTestFiles :: PDPConfig IO -> (PDPHandle -> IO ()) -> [FilePath] -> IO [Either String [(Text, TestResults)]]
runTestFiles conf k = traverse (runTestFile conf k)

data Args = Args
  { systemAssn :: FilePath
  , testFiles  :: [FilePath]
  , otherAssns :: [FilePath]
  } deriving (Eq, Show)

parseArgs :: Opts.Parser Args
parseArgs = Args <$> saParser <*> tfParser <*> oaParser
  where saParser = strOption $ fold saMods
        saMods   = [short 's'
                   , long "system"
                   , value "system.ava"
                   , showDefault
                   , help "file containing the system assertion"]
        tfParser = many . strOption $ fold tfMods
        tfMods   = [short 't'
                   , long "test"
                   , help "files containing tests to run"]
        oaParser = many . strArgument $ fold oaMods
        oaMods   = [help "assertions to load (will be named after their filename)"]



type Repl a = HaskelineT (ReaderT PDPHandle IO) a

cmd :: Command Repl
cmd q = do
  let parsed = parseQuery "<interactive>" (fromString q)
  handle <- ask
  facts  <- liftIO $ readIORef appFacts
  case parsed of
    Left err -> liftIO $ putStrLn err
    Right (Lit (Pred p _) args) ->
      liftIO (runDetailedQuery handle facts p args) >>= either (liftIO . putStrLn . show) putAnswers

-- | TODO: repl options a la ghci's @+t@.
putAnswers :: MonadIO m => DetailedQueryResults -> m ()
putAnswers DetailedResults {..} = liftIO $ putResults results *> putStats
  where putResults [] = putStrLn "no."
        putResults rs = putFacts rs
        putStats      = putStrLn $ "(" <> depthUsage <> " fuel, " <> breadthUsage <> " answers)"
        depthUsage    = show (initialDepth   - remainingDepth)   <> "/" <> show initialDepth
        breadthUsage  = show (initialBreadth - remainingBreadth) <> "/" <> show initialBreadth

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
    submitted <- submitFile handle Nothing path []
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
  let hdr  = "\n\n;; facts written above will be added to the 'application' assertion"
      body = unlines . fmap (prettyString @(Rule TextVar) . factToRule) $ currentFacts

  newSource <- liftIO $ readEditorWith (hdr <> "\n\n" <> body)

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

