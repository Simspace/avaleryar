{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE QuasiQuotes  #-}


module ModeCheckSpec where

import Control.Monad
import Data.Coerce
import Data.Either
import Data.Foldable
import Data.Text        (Text, unpack)
import System.Directory
import System.FilePath

import Language.Avaleryar.ModeCheck
import Language.Avaleryar.Parser
import Language.Avaleryar.PrettyPrinter
import Language.Avaleryar.Syntax

import Fixtures

import Test.Hspec

testModeCheck :: HasCallStack => [Rule RawVar] -> Either Text ()
testModeCheck rules = modeCheck testNativeModes (fmap coerce rules)

-- | These are verbose to get more coverage of the error-message generation
wellModed, illModed :: HasCallStack => [Rule RawVar] -> Expectation
wellModed rules = case testModeCheck rules of
                    Left  !err -> expectationFailure $ unpack err
                    Right !()   -> pure ()

illModed rules = case testModeCheck rules of
                    Left  !err -> pure ()
                    Right !()   -> expectationFailure "no mode error reported"


spec :: Spec
spec = do
  describe "mode checking" $ do
    it "passes for example files" $ do
      files <- filter ((== ".ava") . takeExtension) <$> listDirectory exampleDir
      parsed <- traverse (parseFile . exampleFile) files
      when (null files) $ expectationFailure ("no .ava files in example directory: " <> exampleDir)
      for_ parsed $ \case
         Right parsed -> wellModed parsed
         Left  err    -> expectationFailure err

    it "complains about ill-moded, non-native rules" $ do
      illModed  [rls| may(?x).                     |] -- FV in rule head
      illModed  [rls| may(?x, ?y) :- may(?x).      |] -- FV in rule head
      illModed  [rls| may(?x) :- ?x says may(?x).  |] -- FV in assertion position
      wellModed [rls| may(read).                   |] -- Facts are well moded
      wellModed [rls| may(read) :- a says may(?x). |]

    it "complains about missing native assertions and predicates" $ do
      illModed [rls| foo(?a) :- :not-a-thing says foo(?a). |] -- unbound assertion
      illModed [rls| foo(?a) :- :prim says foo(?a).        |] -- unbound predicate

    it "respects mode-restricted predicates" $ do
      illModed  [rls| foo(?a) :- :prim says rev(?a, a-is-free). |] -- rev(+, -)
      wellModed [rls| foo(?a) :- :prim says rev(a-is-free, ?a). |]
      wellModed [rls| foo(?a) :- :prim says cpu-time(?a).       |] -- cpu-time(-)

      illModed  [rls| foo(?a) :- :prim says silly(?n, ?a, ?b).  |] -- silly(+, -, -)
      wellModed [rls| foo(?a) :- someone says something(?b),
                                 :prim says silly(5, ?a, ?b).   |]

    it "works on the examples from the paper" $ do
      -- Goofus doesn't ground out his variables...
      -- ...Gallant is careful not to try to enumerate the universe
      illModed  [rls| may(?access) :- application says ip-address(?IP),
                                      application says ip-of(?IP, "192.168.0.0/8"),
                                      ?admin says may(?access).|]
      wellModed [rls| may(?access) :- application says ip-address(?IP),
                                      application says ip-of(?IP, "192.168.0.0/8"),
                                      administrator(?admin), ; need to ground out ?admin
                                      ?admin says may(?access).|]
      illModed  [rls| may(?access) :- application says user(?user),
                                      super-user(?user). |] -- ?access free in head

      wellModed [rls| may(?access) :- application says user(?user),
                                      super-user(?user),
                                      known-access(?access). ; ground out ?access
                      known-access(read). ; not actually necessary for the test
                      known-access(write). |]

      -- ACL style would require us to be able to enumerate all the resources, which would be bad.
      illModed  [rls| may(?user, ?access, ?resource) :- super-user(?user),
                                                        known-access(?access). |]
