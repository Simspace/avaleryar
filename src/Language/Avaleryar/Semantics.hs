{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module Language.Avaleryar.Semantics where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State
import           Data.Bool
import           Data.Foldable
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.String
import           Data.Text            (Text, pack, unpack)
import           Data.Void            (Void)

import Control.Monad.FBackTrackT

import Language.Avaleryar.Syntax

import qualified Data.Text as T
import Debug.Trace


newtype RulesDb  m = RulesDb  { unRulesDb  :: Map Value (Map Pred (Lit EVar -> AvaleryarT m ())) }
  deriving (Semigroup, Monoid)
newtype NativeDb m = NativeDb { unNativeDb :: Map Text (Map Pred (Lit EVar -> AvaleryarT m ())) }
  deriving (Semigroup, Monoid)

-- TODO: newtype harder (newtype RuleAssertion c = ..., newtype NativeAssertion c = ...)
data Db m = Db
  { rulesDb  :: RulesDb  m
  , nativeDb :: NativeDb m
  }

instance Semigroup (Db m) where
  Db rdb ndb <> Db rdb' ndb' = Db (rdb <> rdb') (ndb <> ndb')

instance Monoid (Db m) where
  mempty = Db mempty mempty
  mappend = (<>)

alookup :: (Alternative f, Ord k) => k -> Map k a -> f a
alookup k m = maybe empty pure $ Map.lookup k m

loadRule :: (Monad m) => Value -> Pred -> AvaleryarT m (Lit EVar -> AvaleryarT m ())
loadRule c p = gets (unRulesDb . rulesDb . db) >>= alookup c >>= alookup p

loadNative :: Monad m => Text -> Pred -> AvaleryarT m (Lit EVar -> AvaleryarT m ())
loadNative n p = gets (unNativeDb . nativeDb . db) >>= alookup n >>= alookup p

data RT m = RT
  { env   :: Env
  , epoch :: Epoch
  , db    :: Db m
  }

newtype AvaleryarT m a = AvaleryarT { unAvaleryarT :: StateT (RT m) (Stream m) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadState (RT m), MonadYield)

runAvalaryarT :: Monad m => Int -> Int -> Db m -> AvaleryarT m a -> m [a]
runAvalaryarT x y db = runM (Just x) (Just y)
                     . flip evalStateT (RT mempty 0 db)
                     . unAvaleryarT

lookupEVar :: Monad m => EVar -> AvaleryarT m (Term EVar)
lookupEVar ev = do
  RT {..} <- get
  alookup ev env

lookupVar :: Monad m => TextVar -> AvaleryarT m (Term EVar)
lookupVar v = do
  ev <- (,) <$> gets epoch <*> pure v
  lookupEVar ev

unifyTerm :: (Monad m) => Term EVar -> Term EVar -> AvaleryarT m ()
unifyTerm t t' = do
  ts  <- subst t
  ts' <- subst t'
  unless (ts == ts') $ do
    rt@RT {..} <- get
    case (ts, ts') of
      (Var v, _) -> put rt {env = Map.insert v ts' env}
      (_, Var v) -> put rt {env = Map.insert v ts  env}
      _          -> empty -- ts /= ts', both are values

subst :: Monad m => Term EVar -> AvaleryarT m (Term EVar)
subst val@(Val _)  = pure val
subst var@(Var ev) = gets env >>= maybe (pure var) subst . Map.lookup ev

type Goal = BodyLit EVar

loadResolver :: (Monad m) => ARef EVar -> Pred -> AvaleryarT m (Lit EVar -> AvaleryarT m ())
loadResolver (ARNative n) p = loadNative n p
loadResolver (ARTerm   t) p = do
  Val c <- subst t -- mode checking should assure that assertion references are ground by now
  loadRule c p

resolve :: (Monad m) => Goal -> AvaleryarT m (Lit EVar)
resolve (assn `Says` lit@(Lit p as)) = do
  -- Val c <- subst assn
  -- RT {..} <- get
  -- resolver <- yield' $ (alookup c db >>= alookup p)
  resolver <- yield' $ loadResolver assn p
  resolver lit
  Lit p <$> traverse subst as


-- | A slightly safer version of @'zipWithM_' 'unifyTerm'@ that ensures its argument lists are the
-- same length.
unifyArgs :: Monad m => [Term EVar] -> [Term EVar] -> AvaleryarT m ()
unifyArgs [] []         = pure ()
unifyArgs (x:xs) (y:ys) = unifyTerm x y >> unifyArgs xs ys
unifyArgs _ _           = empty

-- | NB: 'compilePred' doesn't look at the 'Pred' for any of the given rules, it assumes it was
-- given a query that applies, and that the rules it was handed are all for the same predicate.
-- This is not the function you want.  FIXME: Suck less
compilePred :: (Monad m) => [Rule TextVar] -> Lit EVar -> AvaleryarT m ()
compilePred rules (Lit _ qas) = do
  rt@RT {..} <- get
  put rt {epoch = succ epoch}
  let rules' = fmap (epoch,) <$> rules
      go (Rule (Lit _ has) body) = do
        unifyArgs has qas
        traverse_ resolve body
  msum $ go <$> rules'

compileRules :: (Monad m) => [Rule TextVar] -> Map Pred (Lit EVar -> AvaleryarT m ())
compileRules rules = fmap compilePred $ Map.fromListWith (++) [(p, [r]) | r@(Rule (Lit p _) _) <- rules]

query :: (Monad m) => String -> Text -> [Term TextVar] -> AvaleryarT m (Lit EVar)
query assn p args = resolve $ assn' `Says` (Lit (Pred p (length args)) (fmap (fmap (-1,)) args))
  where assn' = case assn of
                  (':':_) -> ARNative (pack assn)
                  _       -> ARTerm . Val $ fromString assn

insertRuleAssertion :: Text -> Map Pred (Lit EVar -> AvaleryarT m ()) -> RulesDb m -> RulesDb m
insertRuleAssertion assn rules = RulesDb . Map.insert (T assn) rules . unRulesDb

---------------------

data Solely a = Solely a

inMode :: Mode TextVar
inMode = In "+"

outMode :: Mode TextVar
outMode = Out "-"

class ToNative a where
  toNative :: Monad m => a -> [Term EVar] -> AvaleryarT m ()
  inferMode :: a -> [Mode TextVar]

instance ToNative () where
  toNative () [] = pure ()
  toNative () _  = empty
  inferMode ()   = []

-- TODO: This is either slick or extremely hokey, figure out which.
instance ToNative Bool where
  toNative b [] = guard b
  toNative _ _  = empty
  inferMode _   = []

instance Valuable a => ToNative (Solely a) where
  toNative (Solely a) args = unifyArgs [val a] args
  inferMode _ = [outMode]

instance (Valuable a, Valuable b) => ToNative (a, b) where
  toNative (a, b) args = unifyArgs [val a, val b] args
  inferMode _ = [outMode, outMode]

instance (Valuable a, Valuable b, Valuable c) => ToNative (a, b, c) where
  toNative (a, b, c) args = unifyArgs [val a, val b, val c] args
  inferMode _ = [outMode, outMode, outMode]

instance (Valuable a, ToNative b) => ToNative (a -> b) where
  toNative f (x:xs) = do
    Val x' <- subst x
    case fromValue x' of
      Just a  -> toNative (f a) xs
      Nothing -> empty
  toNative _ _      = trace "tonative ->" empty
  inferMode f = inMode : inferMode (f undefined)

mkNativePred :: (ToNative a, Monad m) => a -> Lit EVar -> AvaleryarT m ()
mkNativePred f (Lit _ args) = toNative f args

foo :: Monad m => NativeDb m
foo = NativeDb . Map.singleton "base" . Map.fromList $ preds
  where preds = [ (Pred "not=" 2, mkNativePred neq) -- ((/=) @Value))
                , (Pred "even" 1, trace "called even" mkNativePred (even @Int))
                , (Pred "odd"  1, mkNativePred (odd @Int))
                , (Pred "rev" 2, mkNativePred (Solely . T.reverse))]

bar :: Map Text (Map Pred ModedLit)
bar = Map.singleton "base" . Map.fromList $ modes
  where modes = [ (Pred "not=" 2, Lit (Pred "not=" 2) $ fmap Var $ inferMode ((/=) @Value))
                , (Pred "even" 1, Lit (Pred "even" 1) $ fmap Var $ inferMode (even @Int))
                , (Pred "odd"  1, Lit (Pred "odd"  1) $ fmap Var $ inferMode (odd @Int))
                , (Pred "rev" 2,  Lit (Pred "rev" 2)  $ fmap Var $ inferMode (Solely . T.reverse))]

neq :: Value -> Value -> Bool
neq v1 v2 = traceShow (v1, v2) v1 /= v2