module TH where

import Control.Monad.IO.Class
import Data.Foldable
import Language.Haskell.TH
import System.Directory

evil :: Q [Dec]
evil = liftIO $ do
  putStrLn "listing /"
  root <- listDirectory "/"
  traverse_ putStrLn root
  putStrLn "checking /etc/passwd"
  perms <- getPermissions "/etc/passwd"
  if readable perms
    then do
      s <- readFile "/etc/passwd"
      putStrLn s
    else putStrLn "/etc/passwd is not readable"
  return []
