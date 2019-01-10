{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Avaleryar.Parser where

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
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Avaleryar

data ParserSettings c = ParserSettings
  { valueParser :: Parsec Void Text c
  , currentAssertionName :: c
  }

type Parser c = ReaderT (ParserSettings c) (Parsec Void Text)

-- ws :: (MonadParsec e s m, Token s ~ Char) => m ()
ws :: Parser c ()
ws = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser c a -> Parser c a
lexeme = L.lexeme ws

symbol :: Text -> Parser c Text
symbol = L.symbol ws

semi, colon, comma, dot :: Parser c ()
semi  = void $ symbol ";"
colon = void $ symbol ":"
comma = void $ symbol ","
dot   = void $ symbol "."

parens :: Parser c a -> Parser c a
parens = between (symbol "(") (symbol ")")

-- I'm stealing ':' for myself, might take more later
symInit, symCont :: Parser c Char
symInit = letterChar <|> oneOf ("!@$%&*/<=>~_^" :: String)
symCont = symInit <|> digitChar <|> oneOf (".+-?" :: String)

stringLiteral :: Parser c Text
stringLiteral = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

sym :: Parser c Text
sym = lexeme (T.pack <$> go) <?> "symbol"
  where go = (:) <$> symInit <*> many symCont

val :: Parser c c
val = ReaderT $ asks valueParser

ident :: Parser c Text
ident = sym <?> "identifer"

newtype RawVar = RawVar { unRawVar :: Text }
  deriving (Eq, Ord, Read, Show, IsString)

var :: Parser c RawVar
var = RawVar <$> (char '?' *> ident) <?> "variable"

term :: Parser c (Term c RawVar)
term =  Var <$> var <|> Val <$> lexeme val

lit :: Parser c (Lit c RawVar)
lit = label "literal" $ do
  ftor <- ident
  args <- parens (term `sepBy` comma)
  pure $ Lit (Pred ftor (length args)) args

aref :: Parser c (ARef c RawVar)
aref = colon *> (ARNative <$> sym) <|> ARTerm <$> term

currentAssertion :: Parser c (ARef c RawVar)
currentAssertion = ARTerm . Val <$> asks currentAssertionName

bodyLit :: Parser c (BodyLit c RawVar)
bodyLit = Says <$> (try (aref <* symbol "says") <|> currentAssertion) <*> lit

rule :: Parser c (Rule c RawVar)
rule = Rule <$> lit <*> (body <|> dot *> pure [])
  where -- bodyLits = ( (try (term val *> symbol "says") *> lit val) <|> lit val) `sepBy1` comma
        bodyLits = bodyLit `sepBy1` comma
        body = symbol ":-" *> label "rule body" bodyLits <* dot

testParse :: Text -> Text -> Either String [Rule Text RawVar]
testParse assn = first errorBundlePretty . parse go (T.unpack assn)
  where textParser = T.pack <$> some alphaNumChar
        go         = runReaderT (ws *> many rule) (ParserSettings textParser assn)

testParseFile :: FilePath -> IO (Either String [Rule Text RawVar])
testParseFile file = T.readFile file >>= pure . testParse (T.pack file)

avaQQParser :: String -> Either String [Rule Text RawVar]
avaQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where textParser = T.pack <$> some alphaNumChar
        go         = runReaderT (many rule) (ParserSettings textParser "qq")

avaQQ :: QuasiQuoter
avaQQ = qqLiteral avaQQParser 'avaQQParser
