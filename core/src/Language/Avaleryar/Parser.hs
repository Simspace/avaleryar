{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Avaleryar.Parser
  ( -- * Parsers
    parseFile
  , parseFactFile
  , parseFacts
  , parseQuery
  , parseText
    -- * Quasiquoters
  , qry
  , fct
  , rls
  ) where

import           Control.Monad              (void)
import           Control.Monad.Reader
import           Data.Bifunctor             (first)
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Void
import           Language.Haskell.TH.Quote  (QuasiQuoter)
import           QQLiterals
import           System.FilePath            (dropExtension)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Avaleryar.Syntax hiding (lit, fact)

data ParserSettings = ParserSettings { currentAssertionName :: Value }

type Parser = ReaderT ParserSettings (Parsec Void Text)

-- ws :: (MonadParsec e s m, Token s ~ Char) => m ()
ws :: Parser ()
ws = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

colon, comma, dot :: Parser ()
colon = void $ symbol ":"
comma = void $ symbol ","
dot   = void $ symbol "."

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- I'm stealing ':' for myself, might take more later
symInit, symCont :: Parser Char
symInit = letterChar <|> oneOf ("!@$%&*/<=>~_^" :: String)
symCont = symInit <|> digitChar <|> oneOf (".+-?" :: String)

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

sym :: Parser Text
sym = lexeme (T.pack <$> go) <?> "symbol"
  where go = (:) <$> symInit <*> many symCont

value :: Parser Value
value =     I <$> L.signed (pure ()) L.decimal
        <|> T <$> stringLiteral
        <|> T <$> sym -- unquoted symbols
        <|> B <$> (string "#t" *> pure True <|> string "#f" *> pure False) 

ident :: Parser Text
ident = sym <?> "identifer"

var :: Parser RawVar
var = RawVar <$> (char '?' *> ident) <?> "variable"

term :: Parser (Term RawVar)
term =  Var <$> var <|> Val <$> lexeme value

lit :: Parser (Lit RawVar)
lit = label "literal" $ do
  ftor <- ident
  args <- parens (term `sepBy` comma)
  pure $ Lit (Pred ftor (length args)) args

-- | A specialized version of 'lit' that fails faster for facts.  Like 'rule' and unlike 'lit',
-- parses a trailing 'dot'.
fact :: Parser Fact
fact = label "fact" $ do
  ftor <- ident
  args <- fmap Val <$> parens (value `sepBy` comma)
  dot
  pure $ Lit (Pred ftor (length args)) args

aref :: Parser (ARef RawVar)
aref = colon *> (ARNative <$> sym) <|> ARTerm <$> term

currentAssertion :: Parser (ARef RawVar)
currentAssertion = ARTerm . Val <$> asks currentAssertionName

bodyLit :: Parser (BodyLit RawVar)
bodyLit = Says <$> (try (aref <* symbol "says") <|> currentAssertion) <*> lit

rule :: Parser (Rule RawVar)
rule = Rule <$> lit <*> (body <|> dot *> pure [])
  where -- bodyLits = ( (try (term val *> symbol "says") *> lit val) <|> lit val) `sepBy1` comma
        bodyLits = bodyLit `sepBy1` comma
        body = symbol ":-" *> label "rule body" bodyLits <* dot

ruleFile :: Parser [Rule RawVar]
ruleFile = ws *> many rule

factFile :: Parser [Fact]
factFile = ws *> many fact

parseFile :: FilePath -> Maybe (FilePath -> String) -> IO (Either String [Rule RawVar])
parseFile path modAssn = do
  let assn = fromString . maybe dropExtension ($) modAssn $ path
  file <- T.readFile path
  pure . first errorBundlePretty $ parse (runReaderT ruleFile (ParserSettings assn)) path file

parseFactFile :: FilePath -> IO (Either String [Fact])
parseFactFile path = do
  file <- T.readFile path
  pure . first errorBundlePretty $ parse (runReaderT factFile (ParserSettings "application")) path file

parseFacts :: Text -> Either String [Fact]
parseFacts src = first errorBundlePretty $ parse (runReaderT factFile (ParserSettings "application")) "" src

parseText :: Text -> Text -> Either String [Rule RawVar]
parseText assn = first errorBundlePretty . parse go (T.unpack assn)
  where go = runReaderT ruleFile (ParserSettings $ T assn)

parseQuery :: Text -> Text -> Either String Query
parseQuery assn = first errorBundlePretty . parse go (T.unpack assn)
  where go = runReaderT (ws *> fmap (fmap unRawVar) lit) (ParserSettings "qq")

testParseFile :: FilePath -> IO (Either String [Rule RawVar])
testParseFile file = T.readFile file >>= pure . parseText (T.pack file)

rulesQQParser :: String -> Either String [Rule RawVar]
rulesQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = runReaderT (ws *> many rule) (ParserSettings "qq")

queryQQParser :: String -> Either String Query
queryQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = runReaderT (ws *> fmap (fmap unRawVar) lit) (ParserSettings "qq")

factQQParser :: String -> Either String Fact
factQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = runReaderT (ws *> fmap (fmap $ error "variable in fact") lit) (ParserSettings "qq")


rls :: QuasiQuoter
rls = qqLiteral rulesQQParser 'rulesQQParser

qry :: QuasiQuoter
qry = qqLiteral queryQQParser 'queryQQParser

fct :: QuasiQuoter
fct = qqLiteral factQQParser 'factQQParser
