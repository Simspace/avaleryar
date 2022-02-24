{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Avaleryar.Parser
  ( -- * Parsers
    parseFile
  , parseFile'
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
import           Data.Bifunctor             (first)
import           Data.Either                (partitionEithers)
import qualified Data.Interned              as Interned
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Void
import           Language.Haskell.TH.Quote  (QuasiQuoter)
import           QQLiterals
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Avaleryar.Syntax hiding (lit, fact)

type Parser = Parsec Void Text

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
symCont = symInit <|> digitChar <|> oneOf ("+-?" :: String)

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

sym :: Parser Text
sym = lexeme (T.pack <$> go) <?> "symbol"
  where go = (:) <$> symInit <*> many symCont

value :: Parser Value
value =     I <$> L.signed (pure ()) L.decimal
        <|> T <$> fmap Interned.intern stringLiteral
        <|> T <$> fmap Interned.intern sym -- unquoted symbols
        <|> B <$> (string "#t" *> pure True <|> string "#f" *> pure False)

ident :: Parser Text
ident = sym <?> "identifer"

var :: Parser RawVar
var = do
  loc <- getSourcePos
  RawVar <$> (char '?' *> ident) <*> pure loc <?> "variable"

term :: Parser (Term RawVar)
term =  Var <$> var <|> Val <$> lexeme value

lit :: Parser (Lit RawVar)
lit = label "literal" $ do
  ftor <- ident
  args <- concat <$> optional (parens (term `sepBy` comma))
  pure $ Lit (Pred ftor (length args)) args

-- | A specialized version of 'lit' that fails faster for facts.  Like 'rule' and unlike 'lit',
-- parses a trailing 'dot'.
fact :: Parser Fact
fact = label "fact" $ do
  ftor <- ident
  args <- fmap Val <$> parens (value `sepBy` comma)
  dot
  pure $ Lit (Pred ftor (length args)) args

-- | Like 'fact', but without the trailing 'dot'.  FIXME: Suck less.
fact' :: Parser Fact
fact' = label "fact" $ do
  ftor <- ident
  args <- fmap Val <$> parens (value `sepBy` comma)
  pure $ Lit (Pred ftor (length args)) args

aref :: Parser (ARef RawVar)
aref = colon *> (ARNative <$> sym) <|> ARTerm <$> term

currentAssertion :: Parser (ARef RawVar)
currentAssertion = pure ARCurrent

bodyLit :: Parser (BodyLit RawVar)
bodyLit = Says <$> (try (aref <* symbol "says") <|> currentAssertion) <*> lit

rule :: Parser (Rule RawVar)
rule = Rule <$> lit <*> (body <|> dot *> pure [])
  where -- bodyLits = ( (try (term val *> symbol "says") *> lit val) <|> lit val) `sepBy1` comma
        bodyLits = bodyLit `sepBy1` comma
        body = symbol ":-" *> label "rule body" bodyLits <* dot

directive :: Parser Directive
directive = do
  void $ symbol ":-"
  label "directive" $
    Directive <$> fact' <*> fact' `sepBy` comma <* dot

-- ruleFile :: Parser [Rule RawVar]
-- ruleFile = ws *> many rule

factFile :: Parser [Fact]
factFile = ws *> many fact

-- FIXME: Suck less
ruleFile' :: Parser ([Directive], [Rule RawVar])
ruleFile' = ws *> (partitionEithers <$> many (fmap Left directive <|> fmap Right rule))

parseFile' :: FilePath -> IO (Either String ([Directive], [Rule RawVar]))
parseFile' path = do
  file <- T.readFile path
  pure . first errorBundlePretty $ parse ruleFile' path file

parseText' :: Text -> Text -> Either String ([Directive], [Rule RawVar])
parseText' assn = first errorBundlePretty . parse ruleFile' (T.unpack assn)

parseFile :: FilePath -> IO (Either String [Rule RawVar])
parseFile path = fmap snd <$> parseFile' path

parseFactFile :: FilePath -> IO (Either String [Fact])
parseFactFile path = do
  file <- T.readFile path
  pure . first errorBundlePretty $ parse factFile path file

parseFacts :: Text -> Either String [Fact]
parseFacts src = first errorBundlePretty $ parse factFile "" src

parseText :: Text -> Text -> Either String [Rule RawVar]
parseText assn src = snd <$> parseText' assn src

parseQuery :: Text -> Text -> Either String Query
parseQuery assn = first errorBundlePretty . parse go (T.unpack assn)
  where go = ws *> fmap (fmap unRawVar) lit

-- testParseFile :: FilePath -> IO (Either String [Rule RawVar])
-- testParseFile file = T.readFile file >>= pure . parseText (T.pack file)

rulesQQParser :: String -> Either String [Rule RawVar]
rulesQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = ws *> many rule

queryQQParser :: String -> Either String Query
queryQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = ws *> fmap (fmap unRawVar) lit

factQQParser :: String -> Either String Fact
factQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = ws *> fmap (fmap $ error "variable in fact") lit


rls :: QuasiQuoter
rls = qqLiteral rulesQQParser 'rulesQQParser

qry :: QuasiQuoter
qry = qqLiteral queryQQParser 'queryQQParser

fct :: QuasiQuoter
fct = qqLiteral factQQParser 'factQQParser
