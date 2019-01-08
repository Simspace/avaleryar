{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.Avaleryar.Parser where

import           Control.Monad              (void)
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Avaleryar

type Parser = Parsec Void Text

-- ws :: (MonadParsec e s m, Token s ~ Char) => m ()
ws :: Parser ()
ws = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

semi, colon, comma, dot :: Parser ()
semi  = void $ symbol ";"
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

ident :: Parser Text
ident = sym <?> "identifer"

newtype RawVar = RawVar { unRawVar :: Text }
  deriving (Eq, Ord, Read, Show, IsString)

var :: Parser RawVar
var = RawVar <$> (char '?' *> ident) <?> "variable"

term :: Parser c -> Parser (Term c RawVar)
term val =  Var <$> var <|> Val <$> lexeme val

lit :: Parser c -> Parser (Lit c RawVar)
lit val = label "literal" $ do
  ftor <- ident
  args <- parens (term val `sepBy` comma)
  pure $ Lit (Pred ftor (length args)) args

aref :: Parser c -> Parser (ARef c RawVar)
aref val = colon *> (ARNative <$> sym) <|> ARTerm <$> term val

fixmeDefaultAssertion = ARNative "FIXME: default reader thing"

bodyLit :: Parser c -> Parser (BodyLit c RawVar)
bodyLit val = Says <$> (try (aref val <* symbol "says") <|> pure fixmeDefaultAssertion) <*> lit val

rule :: Parser c -> Parser (Rule c RawVar)
rule val = Rule <$> (lit val) <*> (body <|> dot *> pure [])
  where -- bodyLits = ( (try (term val *> symbol "says") *> lit val) <|> lit val) `sepBy1` comma
        bodyLits = bodyLit val `sepBy1` comma
        body = symbol ":-" *> label "rule body" bodyLits <* dot

path = T.pack . unlines $ ["path(?x, ?y) :- path(?x, ?z), edge(?z, ?y).  foo() :- bar().",
        "path(?x, ?y) :- edge(?x, ?y).",
        "edge(1, 2).",
        "edge(2, 3).",
        "edge(3, 4).",
        "edge(3, 1).",
        "edge(1, 5).",
        "edge(5, 4)."]

          
-- testPath = do
--     assertSucc path "path(1, 2)"
--     assertSucc path "path(1, 3)"
--     assertSucc path "path(1, 4)"
--     assertSucc path "path(1, 5)"
--     assertSucc path "path(2, 1)"
--     assertSucc path "path(2, 3)"
--     assertSucc path "path(2, 4)"
--     assertSucc path "path(2, 5)"
--     assertSucc path "path(3, 1)"
--     assertSucc path "path(3, 2)"
--     assertSucc path "path(3, 4)"
--     assertSucc path "path(3, 5)"
--     assertFail path "path(4, 1)"
--     assertFail path "path(4, 2)"
--     assertFail path "path(4, 3)"
--     assertFail path "path(4, 5)"
--     assertFail path "path(5, 1)"
--     assertFail path "path(5, 2)"
--     assertFail path "path(5, 3)"
--     assertSucc path "path(5, 4)"
