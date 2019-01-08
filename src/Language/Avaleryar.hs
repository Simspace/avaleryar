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
import           Control.Monad.Fail
import           Control.Monad.State
import           Data.Foldable
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text, unpack)
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

-- newtype IntVar = IntVar { getIntVar :: Int }
--   deriving (Eq, Ord, Read, Show, Num, Enum)
type TextVar = Text
type EVar = (Epoch, TextVar)

type Env c = Map EVar (Term c EVar)
-- type Db m c = Map c (Map Pred (Lit c EVar -> AvaleryarT c m ()))

-- TODO: newtype harder (newtype RuleAssertion c = ..., newtype NativeAssertion c = ...)
data Db m c = Db
  { rulesDb  :: Map c    (Map Pred (Lit c EVar -> AvaleryarT c m ()))
  , nativeDb :: Map Text (Map Pred (Lit c EVar -> AvaleryarT c m ()))
  }

instance Ord c => Semigroup (Db m c) where
  Db rdb ndb <> Db rdb' ndb' = Db (rdb <> rdb') (ndb <> ndb')

instance Ord c => Monoid (Db m c) where
  mempty = Db mempty mempty
  mappend = (<>)

alookup :: (Alternative f, Ord k) => k -> Map k a -> f a
alookup k m = maybe empty pure $ Map.lookup k m

loadRule :: (Value c, Monad m) => c -> Pred -> AvaleryarT c m (Lit c EVar -> AvaleryarT c m ())
loadRule c p = gets (rulesDb . db) >>= alookup c >>= alookup p

loadNative :: Monad m => Text -> Pred -> AvaleryarT c m (Lit c EVar -> AvaleryarT c m ())
loadNative n p = gets (nativeDb . db) >>= alookup n >>= alookup p

data RT m c = RT {
    env   :: Env c
  , epoch :: Epoch
  , db    :: Db m c
  }

newtype AvaleryarT c m a = AvaleryarT { unAvaleryarT :: StateT (RT m c) (Stream m) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadState (RT m c), MonadYield)

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

compileRules :: (Value c, Monad m) => [Rule c TextVar] -> Lit c EVar -> AvaleryarT c m ()
compileRules rules (Lit _ qas) = do
  rt@RT {..} <- get
  put rt {epoch = succ epoch}
  let rules' = fmap (epoch,) <$> rules
      go (Rule (Lit _ has) body) = do
        zipWithM_ unifyTerm has qas
        traverse_ resolve body
  msum $ go <$> rules'

palindromic :: Monad m => Lit Text EVar -> AvaleryarT Text m ()
palindromic (Lit (Pred "palindromic" 1) [mp]) = do
  Val v <- subst mp
  guard (show v == reverse (show v))
palindromic lit = mzero

data Instantiation v = Free v | Ground v
  deriving (Eq, Ord, Read, Show)

data Mode v = In v | Out v
  deriving (Eq, Ord, Read, Show)



-------------------------------------------------------------------------------------
showEVar :: EVar -> String
showEVar (Epoch e, v) = unpack v ++ "_" ++ show e

showEnv :: Show c => Env c -> String
showEnv = unwords . fmap go . Map.toList
  where go (k, v) = showEVar k ++ " = " ++ show v

data ConcRule = ConcLit :- [ConcLit]
data ConcLit  = Text :/ [Text]

parseLit :: ConcLit -> Lit Text TextVar
parseLit (p :/ as) = Lit (Pred p $ length as) [termify a | a <- as]
  where termify a = case unpack a of
                      ('?':_) -> Var a
                      _       -> Val a

parseRule :: ConcRule -> Rule Text TextVar
parseRule (hd :- body) = Rule (parseLit hd) (Says (ARTerm $ Val "system") . parseLit <$> body)

-- mkDb :: [Rule Text TextVar] -> Map Text (Map Pred [Rule Text TextVar])
mkDb :: Monad m => [Rule Text TextVar] -> Db m Text
mkDb rules = flip Db mempty . Map.singleton "system" . fmap compileRules $ Map.fromListWith (++) [(p, [r]) | r@(Rule (Lit p _) _) <- rules]

testStuff x y db q = runM (Just x) (Just y) . flip evalStateT (RT mempty 0 db) . unAvaleryarT . resolve . fmap (-1,) . Says (ARTerm $ Val "system") . parseLit $ q


{-
-- b is the disjunction of c, an infinite success, and d.  None of c's
-- successes lead to a success for a, but d is still reached.
testFairDisj    = assertSucc ["a(?x) :- b(?x), d(?x).",
                              "b(?x) :- c(?x).",
                              "b(?x) :- d(?x).",
                              "c(0).",
                              "c(?x) :- c(?x).",
                              "d(1)."]
                              "a(?x)"
-}

tfd :: Monad m => Db m Text
tfd = mkDb $ fmap parseRule [
        "a" :/ ["?x"] :- ["b" :/ ["?x"], "d" :/ ["?x"]]
      , "b" :/ ["?x"] :- ["c" :/ ["?x"]]
      , "b" :/ ["?x"] :- ["d" :/ ["?x"]]
      , "c" :/ ["0"]  :- []
      , "c" :/ ["?x"] :- ["c" :/ ["?x"]]
      , "d" :/ ["1"]  :- []

      ]



{-
-- b gives two answers, c gives infinite success for both, d rejects b's
-- first answer, but we still get to the second.
testFairConj    = assertSucc ["a(?x) :- b(?x), c(?y), d(?x).",
                              "b(0).",
                              "b(1).",
                              "c(0).",
                              "c(?x) :- c(?x).",
                              "d(1)."]
                              "a(?x)"
-}

tfc :: Monad m => Db m Text
tfc = mkDb $ fmap parseRule [
        "a" :/ ["?x"] :- ["b" :/ ["?x"], "c" :/ ["?"], "d" :/ ["?x"]]
      , "b" :/ ["0"]  :- []
      , "b" :/ ["1"]  :- []
      , "c" :/ ["0"]  :- []
      , "c" :/ ["?x"] :- ["c" :/ ["?x"]]
      , "d" :/ ["1"]  :- []
      ]

{-

path = ["path(?x, ?y) :- path(?x, ?z), edge(?z, ?y).",
        "path(?x, ?y) :- edge(?x, ?y).",
        "edge(1, 2).",
        "edge(2, 3).",
        "edge(3, 4).",
        "edge(3, 1).",
        "edge(1, 5).",
        "edge(5, 4)."]

-}

tp :: Monad m => Db m Text
tp = mkDb $ fmap parseRule [
       "path" :/ ["?x", "?y"] :- ["path" :/ ["?x", "?z"], "edge" :/ ["?z", "?y"]]
     , "path" :/ ["?x", "?y"] :- ["edge" :/ ["?x", "?y"]]
     , "edge" :/ ["1", "2"]   :- []
     , "edge" :/ ["2", "3"]   :- []
     , "edge" :/ ["3", "4"]   :- []
     , "edge" :/ ["3", "1"]   :- []
     , "edge" :/ ["1", "5"]   :- []
     , "edge" :/ ["5", "4"]   :- []
     ]

{-
testPath = do
    assertSucc path "path(1, 2)"
    assertSucc path "path(1, 3)"
    assertSucc path "path(1, 4)"
    assertSucc path "path(1, 5)"
    assertSucc path "path(2, 1)"
    assertSucc path "path(2, 3)"
    assertSucc path "path(2, 4)"
    assertSucc path "path(2, 5)"
    assertSucc path "path(3, 1)"
    assertSucc path "path(3, 2)"
    assertSucc path "path(3, 4)"
    assertSucc path "path(3, 5)"
    assertFail path "path(4, 1)"
    assertFail path "path(4, 2)"
    assertFail path "path(4, 3)"
    assertFail path "path(4, 5)"
    assertFail path "path(5, 1)"
    assertFail path "path(5, 2)"
    assertFail path "path(5, 3)"
    assertSucc path "path(5, 4)"

-}

tpqs = ["path" :/ [x, y] | (x, y) <- paths]
  where paths = [ ("1", "2")
                , ("1", "3")
                , ("1", "4")
                , ("1", "5")
                , ("2", "1")
                , ("2", "3")
                , ("2", "4")
                , ("2", "5")
                , ("3", "1")
                , ("3", "2")
                , ("3", "4")
                , ("3", "5")
                , ("4", "1")
                , ("4", "2")
                , ("4", "3")
                , ("4", "5")
                , ("5", "1")
                , ("5", "2")
                , ("5", "3")
                , ("5", "4")]

