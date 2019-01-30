{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module Language.Avaleryar where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State
import           Data.Bool
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text, unpack, pack)
import           Data.Void           (Void)

import Control.Monad.FBackTrackT

import Debug.Trace

data Val = T Text | I Int deriving (Eq, Ord, Read, Show)

data Pred = Pred Text Int deriving (Eq, Ord, Read, Show)

data Term c v = Val c | Var v deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Lit c v = Lit Pred [Term c v] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ARef c v = ARNative Text | ARTerm (Term c v) deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- give unqualified body literals the "current" assertion
data BodyLit c v = Says (ARef c v) (Lit c v)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data Rule c v = Rule (Lit c v) [BodyLit c v]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type Fact c = Lit c Void

type Value c = (Ord c, Show c)

newtype Epoch = Epoch { getEpoch :: Int }
  deriving (Eq, Ord, Read, Show, Num, Enum)

type TextVar = Text
type EVar = (Epoch, TextVar)

type Env c = Map EVar (Term c EVar)

newtype RulesDb  m c = RulesDb  { unRulesDb  :: Map c (Map Pred (Lit c EVar -> AvaleryarT c m ())) }
  deriving (Semigroup, Monoid)
newtype NativeDb m c = NativeDb { unNativeDb :: Map Text (Map Pred (Lit c EVar -> AvaleryarT c m ())) }
  deriving (Semigroup, Monoid)


-- TODO: newtype harder (newtype RuleAssertion c = ..., newtype NativeAssertion c = ...)
data Db m c = Db
  { rulesDb  :: RulesDb  m c
  , nativeDb :: NativeDb m c
  }


instance Ord c => Semigroup (Db m c) where
  Db rdb ndb <> Db rdb' ndb' = Db (rdb <> rdb') (ndb <> ndb')

instance Ord c => Monoid (Db m c) where
  mempty = Db mempty mempty
  mappend = (<>)

alookup :: (Alternative f, Ord k) => k -> Map k a -> f a
alookup k m = maybe empty pure $ Map.lookup k m

loadRule :: (Value c, Monad m) => c -> Pred -> AvaleryarT c m (Lit c EVar -> AvaleryarT c m ())
loadRule c p = gets (unRulesDb . rulesDb . db) >>= alookup c >>= alookup p

loadNative :: Monad m => Text -> Pred -> AvaleryarT c m (Lit c EVar -> AvaleryarT c m ())
loadNative n p = gets (unNativeDb . nativeDb . db) >>= alookup n >>= alookup p

data RT m c = RT {
    env   :: Env c
  , epoch :: Epoch
  , db    :: Db m c
  }

newtype AvaleryarT c m a = AvaleryarT { unAvaleryarT :: StateT (RT m c) (Stream m) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadState (RT m c), MonadYield)


runAvalaryarT :: Monad m => Int -> Int -> Db m c -> AvaleryarT c m a -> m [a]
runAvalaryarT x y db = runM (Just x) (Just y)
                     . flip evalStateT (RT mempty 0 db)
                     . unAvaleryarT

lookupEVar :: Monad m => EVar -> AvaleryarT c m (Term c EVar)
lookupEVar ev = do
  RT {..} <- get
  alookup ev env

lookupVar :: Monad m => TextVar -> AvaleryarT c m (Term c EVar)
lookupVar v = do
  ev <- (,) <$> gets epoch <*> pure v
  lookupEVar ev

unifyTerm :: (Value c, Monad m) => Term c EVar -> Term c EVar -> AvaleryarT c m ()
unifyTerm t t' = do
  ts  <- subst t
  ts' <- subst t'
  unless (ts == ts') $ do
    rt@RT {..} <- get
    case (ts, ts') of
      (Var v, _) -> put rt {env = Map.insert v ts' env}
      (_, Var v) -> put rt {env = Map.insert v ts  env}
      _          -> empty -- ts /= ts', both are values

subst :: Monad m => Term c EVar -> AvaleryarT c m (Term c EVar)
subst val@(Val _)  = pure val
subst var@(Var ev) = gets env >>= maybe (pure var) subst . Map.lookup ev

type Goal c = BodyLit c EVar

loadResolver :: (Value c, Monad m) => ARef c EVar -> Pred -> AvaleryarT c m (Lit c EVar -> AvaleryarT c m ())
loadResolver (ARNative n) p = loadNative n p
loadResolver (ARTerm   t) p = do
  Val c <- subst t -- mode checking should assure that assertion references are ground by now
  loadRule c p

resolve :: (Value c, Monad m) => Goal c -> AvaleryarT c m (Lit c EVar)
resolve (assn `Says` lit@(Lit p as)) = do
  -- Val c <- subst assn
  -- RT {..} <- get
  -- resolver <- yield' $ (alookup c db >>= alookup p)
  resolver <- yield' $ loadResolver assn p
  resolver lit
  Lit p <$> traverse subst as


-- | NB: 'compilePred' doesn't look at the 'Pred' for any of the given rules, it assumes it was
-- given a query that applies, and that the rules it was handed are all for the same predicate.
-- This is not the function you want.  FIXME: Suck less
compilePred :: (Value c, Monad m) => [Rule c TextVar] -> Lit c EVar -> AvaleryarT c m ()
compilePred rules (Lit _ qas) = do
  rt@RT {..} <- get
  put rt {epoch = succ epoch}
  let rules' = fmap (epoch,) <$> rules
      go (Rule (Lit _ has) body) = do
        zipWithM_ unifyTerm has qas
        traverse_ resolve body
  msum $ go <$> rules'

compileRules :: (Value c, Monad m) => [Rule c TextVar] -> Map Pred (Lit c EVar -> AvaleryarT c m ())
compileRules rules = fmap compilePred $ Map.fromListWith (++) [(p, [r]) | r@(Rule (Lit p _) _) <- rules]

palindromic :: Monad m => Lit Text EVar -> AvaleryarT Text m ()
palindromic (Lit (Pred "palindromic" 1) [mp]) = do
  Val v <- subst mp
  guard (show v == reverse (show v))
palindromic lit = mzero

