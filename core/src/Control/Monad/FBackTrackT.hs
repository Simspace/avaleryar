{-# OPTIONS_GHC -w #-}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{- Haskell98! -}

-- Simple Fair back-tracking monad TRANSFORMER
-- Made by `transforming' the stand-alone monad from FBackTrack.hs,
-- which, in turn, is based on the Scheme code book-si,
-- `Stream implementation, with incomplete' as of Feb 18, 2005
--
-- The transformatiion from a stand-alone Stream monad to a monad transformer
-- is not at all similar to the trick described in Ralf Hinze's ICFP'00 paper,
-- Deriving backtracking monad transformers.

-- Haddock doesn't like @-- $Id@: $Id: FBackTrackT.hs,v 1.1.0.1 2005/10/31 22:34:25 oleg Exp oleg $

module Control.Monad.FBackTrackT
  ( Stream
  , yield
  , runM
  , runM'
  , observe
  , SG
  , MonadYield(..)
  ) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans

data StreamE m a
  = Nil
  | One a
  | Choice a (Stream m a)
  | Incomplete (Stream m a)
    deriving (Functor)

newtype Stream m a = Stream { unStream :: m (StreamE m a) }
  deriving (Functor)

instance Monad m => Applicative (Stream m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Stream m) where
  return = Stream . return . One

  m >>= f = Stream (unStream m >>= bind)
    where
    bind Nil            = return Nil
    bind (One a)        = unStream $ f a
    bind (Choice a r)   = unStream $ f a `mplus` (yield (r >>= f))
    bind (Incomplete i) = return $ Incomplete (i >>= f)

yield :: Monad m => Stream m a -> Stream m a
yield = Stream . return . Incomplete

instance Monad m => Alternative (Stream m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (Stream m) where
  mzero = Stream $ return Nil

  mplus m1 m2 = Stream (unStream m1 >>= mplus')
   where
   mplus' Nil          = return $ Incomplete m2
   mplus' (One a)      = return $ Choice a m2
   mplus' (Choice a r) = return $ Choice a (mplus m2 r) -- interleaving!
   --mplus' (Incomplete i) = return $ Incomplete (mplus i m2)
   mplus' r@(Incomplete i) = unStream m2 >>= \r' ->
      case r' of
        Nil          -> return r
        One b        -> return $ Choice b i
        Choice b r'  -> return $ Choice b (mplus i r')
        -- Choice _ _ -> Incomplete (mplus r' i)
        Incomplete j -> return . Incomplete . yield $ mplus i j

instance Monad m => Fail.MonadFail (Stream m) where
  fail _ = mzero

instance MonadTrans Stream where
    lift m = Stream (m >>= return . One)

instance MonadIO m => MonadIO (Stream m) where
    liftIO = lift . liftIO

instance MonadState s m => MonadState s (Stream m) where
  get  = lift get
  put s = lift (put s)

-- run the Monad, to a specific depth, and give at most
-- specified number of answers. The monad `m' may be strict (like IO),
-- so we can't count on the laziness of the `[a]'
runM :: Monad m => Maybe Int -> Maybe Int -> Stream m a -> m [a]
runM d b s = runM' d b s >>= \(_, _, as) -> pure as

-- Amended 2020/11/16: returns the remaining fuel in addition to the results.
runM' :: Monad m => Maybe Int -> Maybe Int -> Stream m a -> m (Maybe Int, Maybe Int, [a])
runM' d b@(Just 0)  _ = return (d, b, []) -- out of breath
runM' d b         m = unStream m >>= go d b
  where go d b   Nil                 = return (d, b, [])
        go d b (One a)               = return (d, b, [a])
        go d b (Choice a r)          = do (d', b', t) <- runM' d (liftM pred b) r; return (d', b', a:t)
        go d@(Just 0) b (Incomplete r) = return (d, b, []) -- exhausted depth
        go d b (Incomplete r)        = runM' (liftM pred d) b r

-- Don't try the following with the regular List monad or List comprehension!
-- That would diverge instantly: all `i', `j', and `k' are infinite
-- streams

pythagorean_triples :: MonadPlus m => m (Int,Int,Int)
pythagorean_triples =
    let number = (return 0) `mplus` (number >>= return . succ) in
    do
    i <- number
    guard $ i > 0
    j <- number
    guard $ j > 0
    k <- number
    guard $ k > 0
    guard $ i*i + j*j == k*k
    return (i,j,k)

-- If you run this in GHCi, you can see that Indetity is a lazy monad
-- and IO is strict: evaluating `test' prints the answers as they are computed.
-- OTH, testio runs silently for a while and then prints all the answers
-- at once
test = runIdentity $ runM Nothing (Just 7) pythagorean_triples
testio = runM Nothing (Just 7) pythagorean_triples >>= print


-- The following code is not in general MonadPlus: it uses Incomplete
-- explicitly. But it supports left recursion! Note that in OCaml, for example,
-- we _must_ include that Incomplete data constructor to make
-- the recursive definition well-formed.
-- The code does *not* get stuck in the generation of primitive tuples
-- like (0,1,1), (0,2,2), (0,3,3) etc.
pythagorean_triples' :: Monad m => Stream m (Int,Int,Int)
pythagorean_triples' =
    let number = (yield number >>= return . succ) `mplus` return 0  in
    do
    i <- number
    j <- number
    k <- number
    guard $ i*i + j*j == k*k
    return (i,j,k)

test'   = runIdentity $ runM Nothing (Just 27) pythagorean_triples'
testio' = runM Nothing (Just 27) pythagorean_triples' >>= print

pythagorean_triples'' :: Stream IO (Int,Int,Int)
pythagorean_triples'' =
    let number = (yield number >>= return . succ) `mplus` return 0  in
    do
    i <- number
    j <- number
    k <- number
    liftIO $ print (i,j,k)
    guard $ i*i + j*j == k*k
    return (i,j,k)

testio'' = runM Nothing (Just 7) pythagorean_triples'' >>= print

-- a serious test of left recursion (due to Will Byrd)
flaz x = yield (flaz x) `mplus` (yield (flaz x) `mplus` if x == 5 then return x else mzero)
test_flaz = runIdentity $ runM Nothing (Just 15) (flaz 5)

-- FBackTrackT implements LogicT
type SG = Stream

-- instance LogicT Stream where
--   msplit m = Stream (unStream m >>= check)
--    where
--    check Nil            = return . One $ Nothing
--    check (One x)        = return . One $ Just (x,mzero)
--    check (Choice x m)   = return . One $ Just (x,m)
--    check (Incomplete m) = return . Incomplete $ msplit m

-- Hinze's `observe' -- the opposite of `lift'
--	observe . lift == id

observe :: Fail.MonadFail m => Stream m a -> m a
observe m = unStream m >>= pick1
  where pick1 Nil            = fail "no anwers"
        pick1 (One a)        = return a
        pick1 (Choice a _)   = return a
        pick1 (Incomplete m) = observe m

class Monad m => MonadYield m where
  yield' :: m a -> m a

instance Monad m => MonadYield (Stream m) where
  yield' = yield

instance MonadYield m => MonadYield (StateT s m) where
  yield' (StateT sma) = StateT (yield' . sma)
