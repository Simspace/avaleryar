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
import           System.Exit                  (exitFailure)
import           System.Console.Repline       as RL hiding (banner, options)
import           System.IO                    (hPutStrLn, stderr)
import           System.IO.Unsafe
import           System.ReadEditor            (readEditorWith)
import           Text.PrettyPrint.Leijen.Text (Pretty(..))
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
replWithHandle :: PDPConfig -> (PDPHandle -> IO ()) -> IO ()
replWithHandle conf k = do
  let complete = Prefix (wordCompleter byWord) commandMatcher
  handle <- newHandle conf
  k handle
  runReaderT (evalRepl
                banner
                cmd
                options
                commandChar
                multilineStr
                complete
                ini
                fin) handle

-- | As 'replWithHandle', doing no additional configuration.
repl :: PDPConfig -> IO ()
repl conf = replWithHandle conf mempty

main :: IO ()
main = do
  Args {..}  <- execParser (info parseArgs mempty)
  conf <- pdpConfig demoNativeDb systemAssn >>= either diePretty pure
  let loadAssns h = for_ otherAssns $ either diePretty pure <=< unsafeSubmitFile h Nothing
      displayResults = traverse_ $ either putStrLn (traverse_ $ uncurry putTestResults)

  traverse_ (loadApplication <=< liftIO . readFile) appAssn

  if   null testFiles
  then replWithHandle conf loadAssns
  else runTestFiles conf loadAssns testFiles >>= displayResults

runTestFiles :: PDPConfig -> (PDPHandle -> IO ()) -> [FilePath] -> IO [Either String [(Text, TestResults)]]
runTestFiles conf k = traverse (runTestFile conf k)

data Args = Args
  { systemAssn :: FilePath
  , testFiles  :: [FilePath]
  , otherAssns :: [FilePath]
  , appAssn    :: Maybe FilePath
  } deriving (Eq, Show)

parseArgs :: Opts.Parser Args
parseArgs = Args <$> saParser <*> tfParser <*> oaParser <*> afParser
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
        afParser = optional . strOption $ fold afMods
        afMods   = [short 'a'
                   , long "application"
                   , help "file containing facts for the application assertion"]



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

banner :: MultiLine -> Repl String
banner _ = pure "-? "

options :: Options Repl
options = [ ("load", load)
          , ("dump", dump)
          , ("app",  app)]

appFacts :: IORef [Fact]
appFacts = unsafePerformIO $ newIORef []
{-# NOINLINE appFacts #-}

load :: FilePath -> Repl ()
load path = dontCrash $ do
  handle <- ask
  liftIO $ do
    submitted <- submitFile handle Nothing path []
    case submitted of
      Left err -> putStrLn $ path ++ ": " ++ show err
      Right () -> pure ()

dump :: String -> Repl ()
dump assns = do
  dumped <- ask >>= liftIO . dumpDb
  let assns' | null assns = Map.keys dumped
             | otherwise  = fromString <$> words assns -- janky
  for_ (nubOrd assns') $ \assn -> liftIO $ do
    traverse_ (putAssertion assn) $ Map.lookup assn dumped
    liftIO $ putStrLn ""

app :: String -> Repl ()
app _ = do
  currentFacts <- liftIO $ readIORef appFacts
  let hdr  = "\n\n;; facts written above will be added to the 'application' assertion"
      body = unlines . fmap (prettyString @(Rule TextVar) . factToRule) $ currentFacts

  newSource <- liftIO $ readEditorWith (hdr <> "\n\n" <> body)
  liftIO $ loadApplication newSource

-- | Helper for 'app' and the @-a@ argument.  Takes the string containing (concrete-syntax) facts.
loadApplication :: String -> IO ()
loadApplication src = do
  let parsed = parseFacts (fromString src)

  case parsed of
    Left err -> putStrLn err *> putStrLn "failed to load any facts."
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

fin :: Repl ExitDecision
fin = pure Exit

commandChar :: Maybe Char
commandChar = Just ':'

multilineStr :: Maybe String
multilineStr = Nothing

diePretty :: (Pretty a, MonadIO m) => a -> m b
diePretty x = liftIO $ do
  PP.hPutDoc stderr (pretty x)
  hPutStrLn stderr ""
  exitFailure
