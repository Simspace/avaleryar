
module Language.Avaleryar where

import Data.Coerce
import qualified Data.Text.IO as T
import Text.PrettyPrint.Leijen.Text

import Language.Avaleryar.ModeCheck (modeCheck)
import Language.Avaleryar.Parser (parseFile)
import Language.Avaleryar.PrettyPrinter ()
import Language.Avaleryar.Syntax (RawVar(..), TextVar)

foo path = do
  Right stuff <- parseFile path Nothing
  case modeCheck mempty $ fmap (fmap unRawVar) stuff of
    Left err -> T.putStrLn err
    Right _  -> putDoc $ foldMap pretty stuff
