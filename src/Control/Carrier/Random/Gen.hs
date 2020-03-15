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
, RandomC(RandomC)
  -- * Random effect
, module Control.Effect.Random
) where

import           Control.Algebra
import           Control.Applicative (Alternative)
import           Control.Carrier.State.Church
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
runRandom :: Applicative m => g -> RandomC g m a -> m (g, a)
runRandom g = runState (curry pure) g . runRandomC
{-# INLINE runRandom #-}

-- | Run a random computation starting from a given generator and discarding the final generator.
--
-- @
-- 'evalRandom' g ('pure' b) = 'pure' b
-- @
evalRandom :: Applicative m => g -> RandomC g m a -> m a
evalRandom g = evalState g . runRandomC
{-# INLINE evalRandom #-}

-- | Run a random computation starting from a given generator and discarding the final result.
--
-- @
-- 'execRandom' g ('pure' b) = g
-- @
execRandom :: Applicative m => g -> RandomC g m a -> m g
execRandom g = execState g . runRandomC
{-# INLINE execRandom #-}

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomSystem :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomSystem m = liftIO R.newStdGen >>= flip evalRandom m
{-# INLINE evalRandomSystem #-}

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, R.RandomGen g) => Algebra (Random :+: sig) (RandomC g m) where
  alg hdl sig ctx = case sig of
    L Uniform        -> (<$ ctx) <$> state R.random
    L (UniformR r  ) -> (<$ ctx) <$> state (R.randomR r)
    L (Interleave m) -> do
      g2 <- state R.split
      a <- hdl (m <$ ctx)
      a <$ RandomC (put g2)
    R other          -> RandomC (alg (runRandomC . hdl) (R other) ctx)
    where
    state :: (g -> (a, g)) -> RandomC g m a
    state f = RandomC (StateC (\ k -> uncurry (flip k) . f))
  {-# INLINE alg #-}
