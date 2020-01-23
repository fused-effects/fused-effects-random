{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Random.Gen
( -- * Random carrier
  runRandom
, evalRandom
, execRandom
, evalRandomSystem
, RandomC(..)
) where

import           Control.Algebra
import           Control.Applicative (Alternative)
import           Control.Carrier.State.Strict
import           Control.Effect.Random
import           Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class
import qualified System.Random as R (Random(..), RandomGen(..), StdGen, newStdGen)

-- | Run a random computation starting from a given generator.
--
-- @
-- 'runRandom' g ('pure' b) = 'pure' (g, b)
-- @
runRandom :: g -> RandomC g m a -> m (g, a)
runRandom g = runState g . runRandomC

-- | Run a random computation starting from a given generator and discarding the final generator.
--
-- @
-- 'evalRandom' g ('pure' b) = 'pure' b
-- @
evalRandom :: Functor m => g -> RandomC g m a -> m a
evalRandom g = fmap snd . runRandom g

-- | Run a random computation starting from a given generator and discarding the final result.
--
-- @
-- 'execRandom' g ('pure' b) = g
-- @
execRandom :: Functor m => g -> RandomC g m a -> m g
execRandom g = fmap fst . runRandom g

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomSystem :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomSystem m = liftIO R.newStdGen >>= flip evalRandom m

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, Effect sig, R.RandomGen g) => Algebra (Random :+: sig) (RandomC g m) where
  alg = \case
    L (Random       k) -> do
      (a, g') <- RandomC (gets R.random)
      RandomC (put (g' :: g))
      k a
    L (RandomR r    k) -> do
      (a, g') <- RandomC (gets (R.randomR r))
      RandomC (put (g' :: g))
      k a
    L (Interleave m k) -> do
      (g1, g2) <- RandomC (gets R.split)
      RandomC (put (g1 :: g))
      a <- m
      RandomC (put g2)
      k a
    R other            -> RandomC (send (handleCoercible other))
  {-# INLINE alg #-}
