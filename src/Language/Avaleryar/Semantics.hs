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


-- | NB: 'compilePred' doesn't look at the 'Pred' for any of the given rules, it assumes it was
-- given a query that applies, and that the rules it was handed are all for the same predicate.
-- This is not the function you want.  FIXME: Suck less
compilePred :: (Monad m) => [Rule TextVar] -> Lit EVar -> AvaleryarT m ()
compilePred rules (Lit _ qas) = do
  rt@RT {..} <- get
  put rt {epoch = succ epoch}
  let rules' = fmap (epoch,) <$> rules
      go (Rule (Lit _ has) body) = do
        zipWithM_ unifyTerm has qas
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
insertRuleAssertion assn rules = RulesDb . Map.insert (S assn) rules . unRulesDb
