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
