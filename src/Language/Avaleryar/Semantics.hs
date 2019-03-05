{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.Avaleryar.Semantics where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State
import           Data.Foldable
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.String
import           Data.Text            (Text, pack)

import Control.Monad.FBackTrackT

import Language.Avaleryar.Syntax


data NativePred m = NativePred
  { nativePred :: Lit EVar -> AvaleryarT m ()
  , nativeSig  :: ModedLit
  }

newtype RulesDb  m = RulesDb  { unRulesDb  :: Map Value (Map Pred (Lit EVar -> AvaleryarT m ())) }
  deriving (Semigroup, Monoid)
newtype NativeDb m = NativeDb { unNativeDb :: Map Text (Map Pred (NativePred m)) }
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
loadNative n p = gets (unNativeDb . nativeDb . db) >>= alookup n >>= alookup p >>= pure . nativePred

data RT m = RT
  { env   :: Env
  , epoch :: Epoch
  , db    :: Db m
  }

newtype AvaleryarT m a = AvaleryarT { unAvaleryarT :: StateT (RT m) (Stream m) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadState (RT m), MonadYield, MonadIO)

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
subst v@(Val _)    = pure v
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

-- | TODO: Suck less
query' :: Monad m => String -> Lit TextVar -> AvaleryarT m (Lit EVar)
query' assn (Lit (Pred p _) args) = query assn p args

insertRuleAssertion :: Text -> Map Pred (Lit EVar -> AvaleryarT m ()) -> RulesDb m -> RulesDb m
insertRuleAssertion assn rules = RulesDb . Map.insert (T assn) rules . unRulesDb

retractRuleAssertion :: Text -> RulesDb m -> RulesDb m
retractRuleAssertion assn = RulesDb . Map.delete (T assn) . unRulesDb

---------------------

data Solely a = Solely a

inMode :: Mode TextVar
inMode = In "+"

outMode :: Mode TextVar
outMode = Out "-"

class ToNative a where
  toNative :: MonadIO m => a -> [Term EVar] -> AvaleryarT m ()
  inferMode :: [Mode TextVar]

instance ToNative () where
  toNative () [] = pure ()
  toNative () _  = empty
  inferMode     = []

-- TODO: This is either slick or extremely hokey, figure out which.
instance ToNative Bool where
  toNative b [] = guard b
  toNative _ _  = empty
  inferMode     = []

-- TODO: This is also either slick or extremely hokey, figure out which.
instance ToNative a => ToNative [a] where
  toNative as xs = msum [toNative a xs | a <- as]
  inferMode      = inferMode @a

instance Valuable a => ToNative (Solely a) where
  toNative (Solely a) args = unifyArgs [val a] args
  inferMode = [outMode]

instance (Valuable a, Valuable b) => ToNative (a, b) where
  toNative (a, b) args = unifyArgs [val a, val b] args
  inferMode = [outMode, outMode]

instance (Valuable a, Valuable b, Valuable c) => ToNative (a, b, c) where
  toNative (a, b, c) args = unifyArgs [val a, val b, val c] args
  inferMode = [outMode, outMode, outMode]

instance (Valuable a, Valuable b, Valuable c, Valuable d) => ToNative (a, b, c, d) where
  toNative (a, b, c, d) args = unifyArgs [val a, val b, val c, val d] args
  inferMode = [outMode, outMode, outMode, outMode]

instance (Valuable a, Valuable b, Valuable c, Valuable d, Valuable e) => ToNative (a, b, c, d, e) where
  toNative (a, b, c, d, e) args = unifyArgs [val a, val b, val c, val d, val e] args
  inferMode = [outMode, outMode, outMode, outMode, outMode]

instance (Valuable a, Valuable b, Valuable c, Valuable d, Valuable e, Valuable f) => ToNative (a, b, c, d, e, f) where
  toNative (a, b, c, d, e, f) args = unifyArgs [val a, val b, val c, val d, val e, val f] args
  inferMode = [outMode, outMode, outMode, outMode, outMode, outMode]

instance (Valuable a, ToNative b) => ToNative (a -> b) where
  toNative f (x:xs) = do
    Val x' <- subst x
    case fromValue x' of
      Just a  -> toNative (f a) xs
      Nothing -> empty
  toNative _ _      = empty
  inferMode = inMode : inferMode @b

instance ToNative a => ToNative (IO a) where
  toNative ma xs = do
    a <- liftIO ma
    toNative a xs

  inferMode = inferMode @a

mkNativePred :: forall a m. (ToNative a, MonadIO m) => Text -> a -> NativePred m
mkNativePred pn f = NativePred np moded
  where np (Lit _ args) = toNative f args
        modes = inferMode @a
        moded = Lit (Pred pn $ length modes) (Var <$> modes)

mkNativeDb :: Monad m => Text -> [NativePred m] -> NativeDb m
mkNativeDb assn preds = NativeDb . Map.singleton assn $ Map.fromList [(p, np) | np@(NativePred _ (Lit p _)) <- preds]
