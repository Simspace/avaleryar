{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Language.Avaleryar.ModeCheck where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bool
import           Data.Foldable
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text, pack)

import Language.Avaleryar.Syntax

data Instantiation = Free | Ground
  deriving (Eq, Ord, Read, Show)

data ModeEnv = ModeEnv
  { nativeModes  :: Map Text (Map Pred ModedLit)
  , groundedVars :: [TextVar] }

-- TODO: Actually use this type; try pushing mode checking through parsing for pretty errors?
data ModeError = UnboundNativeAssertion Text
               | UnboundNativePredicate Text Pred
               | FVModeRestricted TextVar TextVar -- first var is the free one; TODO: Suck less
               | FVInAssertionPosition TextVar
               | FVInRuleHead TextVar
                 deriving (Eq, Ord, Read, Show)

newtype ModeCheck m a = ModeCheck { unModeCheck :: ExceptT Text (StateT ModeEnv m) a }
  deriving (Functor, Applicative, Monad, MonadError Text)

getMode :: Monad m => BodyLit TextVar -> ModeCheck m ModedLit
getMode (ARTerm _ `Says` lit) = pure . fmap Out $ lit
getMode (ARNative assn `Says` Lit p _) = ModeCheck $ do
  amap <- gets nativeModes
  let missingAssertion = "Unbound native assertion: '" <> assn <> "'"
      missingPredicate = "Unbound native predicate: '" <> displayPred p <> "' in assertion '" <> assn <> "'"
  pmap <- maybe (throwError missingAssertion) pure $ Map.lookup assn amap
  maybe (throwError missingPredicate) pure $ Map.lookup p pmap

getNativeMode :: Monad m => Text -> Pred -> ModeCheck m ModedLit
getNativeMode assn p = ModeCheck $ do
  amap <- gets nativeModes
  let missingAssertion = "Unbound native assertion: '" <> assn <> "'"
      missingPredicate = "Unbound native predicate: '" <> displayPred p <> "' in assertion '" <> assn <> "'"
  pmap <- maybe (throwError missingAssertion) pure $ Map.lookup assn amap
  maybe (throwError missingPredicate) pure $ Map.lookup p pmap

displayPred :: Pred -> Text
displayPred (Pred f n) = f <> "/" <> (pack . show $ n)

ground :: (Monad m, Foldable f) => f TextVar -> ModeCheck m ()
ground vs = ModeCheck (modify go)
  where go env@ModeEnv {..} = env { groundedVars = toList vs <> groundedVars }

grounded :: Monad m => TextVar -> ModeCheck m Bool
grounded v = ModeCheck $ gets (elem v . groundedVars)

modeCheckRule :: Monad m => Rule TextVar -> ModeCheck m ()
modeCheckRule (Rule hd body) = traverse_ modeCheckBody body >> modeCheckHead hd
  where modeCheckBody (ARNative assn `Says` Lit p bas) = do
          Lit _ mas <- getNativeMode assn p
          zipWithM_ modeCheckArg mas bas
        modeCheckBody (ARTerm aref `Says` Lit _ bas) = do
          let freeAssertion v = "variable '" <> v <> "' is free in assertion position"
          case aref of
            Var v -> grounded v >>= bool (throwError $ freeAssertion v) (pure ())
            _     -> pure ()

          traverse_ ground bas


        modeCheckArg (Val _)       a       = ground a -- treat constants like in-mode variables
        modeCheckArg (Var (Out  _)) a       = ground a -- predicates ground in-mode variables
        modeCheckArg (Var (In _)) (Val _) = pure ()
        modeCheckArg (Var (In o)) (Var v) = do
          let modeMismatch = "variable '" <> v <> "' is free in mode-restricted position: '" <> o <> "'"
          isGrounded <- grounded v
          unless isGrounded $ throwError modeMismatch

        modeCheckHead = traverse_ $ \v -> do
          let freeInHead = "variable '" <> v <> "' appears free in rule head"
          isGrounded <- grounded v
          unless isGrounded $ throwError freeInHead

modeCheck :: (Foldable t) => Map Text (Map Pred ModedLit) -> t (Rule TextVar) -> Either Text ()
modeCheck native = traverse_ $ flip evalState (ModeEnv native mempty) . runExceptT . unModeCheck . modeCheckRule

modeCheck' :: (Foldable t) => t (Rule TextVar) -> Either Text ()
modeCheck' = modeCheck mempty
