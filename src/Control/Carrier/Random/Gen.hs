{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  -- * Random effect
, module Control.Effect.Random
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
import           Data.Tuple (swap)
import qualified System.Random as R (Random(..), RandomGen(..), StdGen, newStdGen)

-- | Run a random computation starting from a given generator.
--
-- @
-- 'runRandom' g ('pure' b) = 'pure' (g, b)
-- @
runRandom :: g -> RandomC g m a -> m (g, a)
runRandom g = runState g . runRandomC
{-# INLINE runRandom #-}

-- | Run a random computation starting from a given generator and discarding the final generator.
--
-- @
-- 'evalRandom' g ('pure' b) = 'pure' b
-- @
evalRandom :: Functor m => g -> RandomC g m a -> m a
evalRandom g = fmap snd . runRandom g
{-# INLINE evalRandom #-}

-- | Run a random computation starting from a given generator and discarding the final result.
--
-- @
-- 'execRandom' g ('pure' b) = g
-- @
execRandom :: Functor m => g -> RandomC g m a -> m g
execRandom g = fmap fst . runRandom g
{-# INLINE execRandom #-}

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomSystem :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomSystem m = liftIO R.newStdGen >>= flip evalRandom m
{-# INLINE evalRandomSystem #-}

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, R.RandomGen g) => Algebra (Random :+: sig) (RandomC g m) where
  alg hdl sig ctx = case sig of
    L (Uniform      k) -> state R.random      >>= hdl . (<$ ctx) . k
    L (UniformR r   k) -> state (R.randomR r) >>= hdl . (<$ ctx) . k
    L (Interleave m k) -> do
      g2 <- state R.split
      a <- hdl (m <$ ctx)
      RandomC (put g2)
      hdl (fmap k a)
    R other            -> RandomC (alg (runRandomC . hdl) (R other) ctx)
    where
    state :: (g -> (a, g)) -> RandomC g m a
    state f = RandomC (StateC (pure . swap . f))
  {-# INLINE alg #-}
