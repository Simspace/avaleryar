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
import           System.FilePath            (dropExtension)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Avaleryar.Syntax
import Language.Avaleryar.Value

data ParserSettings c = ParserSettings { currentAssertionName :: c }

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

val :: Value c => Parser c c
val = lift parseValue

ident :: Parser c Text
ident = sym <?> "identifer"

newtype RawVar = RawVar { unRawVar :: Text }
  deriving (Eq, Ord, Read, Show, IsString)

var :: Parser c RawVar
var = RawVar <$> (char '?' *> ident) <?> "variable"

term :: Value c => Parser c (Term c RawVar)
term =  Var <$> var <|> Val <$> lexeme val

lit :: Value c => Parser c (Lit c RawVar)
lit = label "literal" $ do
  ftor <- ident
  args <- parens (term `sepBy` comma)
  pure $ Lit (Pred ftor (length args)) args

aref :: Value c => Parser c (ARef c RawVar)
aref = colon *> (ARNative <$> sym) <|> ARTerm <$> term

currentAssertion :: Parser c (ARef c RawVar)
currentAssertion = ARTerm . Val <$> asks currentAssertionName

bodyLit :: Value c => Parser c (BodyLit c RawVar)
bodyLit = Says <$> (try (aref <* symbol "says") <|> currentAssertion) <*> lit

rule :: Value c => Parser c (Rule c RawVar)
rule = Rule <$> lit <*> (body <|> dot *> pure [])
  where -- bodyLits = ( (try (term val *> symbol "says") *> lit val) <|> lit val) `sepBy1` comma
        bodyLits = bodyLit `sepBy1` comma
        body = symbol ":-" *> label "rule body" bodyLits <* dot

ruleFile :: Value c => Parser c [Rule c RawVar]
ruleFile = ws *> many rule

parseFile :: (Value c) =>
              FilePath
              -> Maybe (FilePath -> String)
              -> IO (Either String [Rule c RawVar])
parseFile path modAssn = do
  let assn = valueFromString . maybe dropExtension ($) modAssn $ path
  file <- T.readFile path
  pure . first errorBundlePretty $ parse (runReaderT ruleFile (ParserSettings assn)) path file
  
  

testParse :: Text -> Text -> Either String [Rule Text RawVar]
testParse assn = first errorBundlePretty . parse go (T.unpack assn)
  where go = runReaderT ruleFile (ParserSettings assn)

testParseFile :: FilePath -> IO (Either String [Rule Text RawVar])
testParseFile file = T.readFile file >>= pure . testParse (T.pack file)

avaQQParser :: String -> Either String [Rule Text RawVar]
avaQQParser = first errorBundlePretty . parse go "qq" . T.pack
  where go = runReaderT (ws *> many rule) (ParserSettings "qq")

avaQQ :: QuasiQuoter
avaQQ = qqLiteral avaQQParser 'avaQQParser
