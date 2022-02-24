{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.Avaleryar.ModeCheck where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bool
import           Data.Foldable
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text, pack)
import           Text.Megaparsec              (sourcePosPretty)
import           Text.PrettyPrint.Leijen.Text (Pretty(..), colon, squotes)

import Language.Avaleryar.Syntax

data ModeEnv = ModeEnv
  { nativeModes  :: Map Text (Map Pred ModedLit)
  , groundedVars :: [RawVar] }

data ModeError
  = UnboundNativeAssertion Text
  | UnboundNativePredicate Text Pred
  | FVModeRestricted RawVar
  | FVInAssertionPosition RawVar
  | FVInRuleHead RawVar
    deriving (Eq, Ord, Show)

instance Pretty ModeError where
  pretty (UnboundNativeAssertion assn)        = "unbound native assertion: " <> squotes (pretty assn)
  pretty (UnboundNativePredicate assn prd)    =
    "unbound native predicate: "
      <> squotes (pretty prd)
      <> " in assertion "
      <> squotes (pretty assn)
  pretty (FVModeRestricted (RawVar v l))      =
    pretty (sourcePosPretty l) <> colon <> "variable: "
      <> squotes (pretty v)
      <> " is free in mode restricted position"
  pretty (FVInAssertionPosition (RawVar v l)) =
    pretty (sourcePosPretty l) <> colon <> "variable: "
      <> squotes (pretty v)
      <> " appears free in assertion position"
  pretty (FVInRuleHead (RawVar v l))          =
    pretty (sourcePosPretty l) <> colon <> "variable "
      <> squotes (pretty v)
      <> " appears free in rule head"


newtype ModeCheck m a = ModeCheck { unModeCheck :: ExceptT ModeError (StateT ModeEnv m) a }
  deriving (Functor, Applicative, Monad, MonadError ModeError)

getNativeMode :: Monad m => Text -> Pred -> ModeCheck m ModedLit
getNativeMode assn p = ModeCheck $ do
  amap <- gets nativeModes
  pmap <- maybe (throwError $ UnboundNativeAssertion assn) pure $ Map.lookup assn amap
  maybe (throwError $ UnboundNativePredicate assn p) pure $ Map.lookup p pmap

displayPred :: Pred -> Text
displayPred (Pred f n) = f <> "/" <> (pack . show $ n)

ground :: (Monad m, Foldable f) => f RawVar -> ModeCheck m ()
ground vs = ModeCheck (modify go)
  where go env@ModeEnv {..} = env { groundedVars = toList vs <> groundedVars }

grounded :: Monad m => RawVar -> ModeCheck m Bool
grounded v = ModeCheck $ gets (elem v . groundedVars)

modeCheckRule :: Monad m => Rule RawVar -> ModeCheck m ()
modeCheckRule (Rule hd body) = traverse_ modeCheckBody body >> modeCheckHead hd
  where modeCheckBody (ARNative assn `Says` Lit p bas) = do
          Lit _ mas <- getNativeMode assn p
          zipWithM_ modeCheckArg mas bas
        modeCheckBody (ARTerm aref `Says` Lit _ bas) = do
          case aref of
            Var v -> grounded v >>= bool (throwError $ FVInAssertionPosition v) (pure ())
            _     -> pure ()

          traverse_ ground bas
        modeCheckBody (ARCurrent `Says` Lit _ bas) = traverse_ ground bas


        modeCheckArg (Val _)        a     = ground a -- treat constants like in-mode variables
        modeCheckArg (Var (Out  _)) a     = ground a -- predicates ground in-mode variables
        modeCheckArg (Var (In _)) (Val _) = pure ()
        modeCheckArg (Var (In _)) (Var v) = do
          isGrounded <- grounded v
          unless isGrounded $ throwError (FVModeRestricted v)

        modeCheckHead = traverse_ $ \v -> do
          isGrounded <- grounded v
          unless isGrounded $ throwError (FVInRuleHead v)

modeCheck :: (Foldable t) => Map Text (Map Pred ModedLit) -> t (Rule RawVar) -> Either ModeError ()
modeCheck native = traverse_ $ flip evalState (ModeEnv native mempty) . runExceptT . unModeCheck . modeCheckRule
