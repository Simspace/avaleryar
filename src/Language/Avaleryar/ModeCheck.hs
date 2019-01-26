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

import Language.Avaleryar

data Instantiation = Free | Ground
  deriving (Eq, Ord, Read, Show)

data Mode v = In v | Out v
  deriving (Eq, Ord, Read, Show)

type ModedLit c = Lit c (Mode TextVar)

data ModeEnv c = ModeEnv
  { nativeModes  :: Map Text (Map Pred (ModedLit c))
  , groundedVars :: [TextVar] }

newtype ModeCheck c m a = ModeCheck { unModeCheck :: ExceptT Text (StateT (ModeEnv c) m) a }
  deriving (Functor, Applicative, Monad, MonadError Text)

getMode :: Monad m => BodyLit c TextVar -> ModeCheck c m (ModedLit c)
getMode (ARTerm _ `Says` lit) = pure . fmap In $ lit
getMode (ARNative assn `Says` Lit p _) = ModeCheck $ do
  amap <- gets nativeModes
  let missingAssertion = "Unbound native assertion: '" <> assn <> "'"
      missingPredicate = "Unbound native predicate: '" <> displayPred p <> "' in assertion '" <> assn <> "'"
  pmap <- maybe (throwError missingAssertion) pure $ Map.lookup assn amap
  maybe (throwError missingPredicate) pure $ Map.lookup p pmap

getNativeMode :: Monad m => Text -> Pred -> ModeCheck c m (ModedLit c)
getNativeMode assn p = ModeCheck $ do
  amap <- gets nativeModes
  let missingAssertion = "Unbound native assertion: '" <> assn <> "'"
      missingPredicate = "Unbound native predicate: '" <> displayPred p <> "' in assertion '" <> assn <> "'"
  pmap <- maybe (throwError missingAssertion) pure $ Map.lookup assn amap
  maybe (throwError missingPredicate) pure $ Map.lookup p pmap

displayPred :: Pred -> Text
displayPred (Pred f n) = f <> "/" <> (pack . show $ n)

ground :: (Monad m, Foldable f) => f TextVar -> ModeCheck c m ()
ground vs = ModeCheck (modify go)
  where go env@ModeEnv {..} = env { groundedVars = toList vs <> groundedVars }

grounded :: Monad m => TextVar -> ModeCheck c m Bool
grounded v = ModeCheck $ gets (elem v . groundedVars)

modeCheckRule :: Monad m => Rule c TextVar -> ModeCheck c m ()
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
        modeCheckArg (Var (In  _)) a       = ground a -- predicates ground in-mode variables
        modeCheckArg (Var (Out _)) (Val _) = pure ()
        modeCheckArg (Var (Out o)) (Var v) = do
          let modeMismatch = "variable '" <> v <> "' is free in mode-restricted position: '" <> o <> "'"
          isGrounded <- grounded v
          unless isGrounded $ throwError modeMismatch

        modeCheckHead = traverse_ $ \v -> do
          let freeInHead = "variable '" <> v <> "' appears free in rule head"
          isGrounded <- grounded v
          unless isGrounded $ throwError freeInHead

modeCheck :: (Foldable t) => Map Text (Map Pred (ModedLit c)) -> t (Rule c TextVar) -> Either Text ()
modeCheck native = traverse_ $ flip evalState (ModeEnv native mempty) . runExceptT . unModeCheck . modeCheckRule

modeCheck' :: (Foldable t) => t (Rule c TextVar) -> Either Text ()
modeCheck' = modeCheck mempty
