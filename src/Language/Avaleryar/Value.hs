{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Avaleryar.Value where

import Data.String
import Data.Text                  (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- type Value c = (Ord c, Show c, IsString c)

class Ord c => Value c where
  parseValue :: Parsec Void Text c

valueFromString :: Value c => String -> c
valueFromString s = maybe (error $ "valueFromString couldn't parse: " <> s) id $ parseMaybe parseValue (pack s)

instance Value Text where
  parseValue = pack <$> some alphaNumChar

instance Value Int where
  parseValue = signed (pure ()) decimal
