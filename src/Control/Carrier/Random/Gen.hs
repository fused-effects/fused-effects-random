{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A carrier for "Control.Effect.Random".'Random' implemented using 'R.RandomGen'.
--
-- @since 1.1
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
--
-- @since 1.0
runRandom :: Applicative m => g -> RandomC g m a -> m (g, a)
runRandom g = runState (curry pure) g . runRandomC
{-# INLINE runRandom #-}

-- | Run a random computation starting from a given generator and discarding the final generator.
--
-- @
-- 'evalRandom' g ('pure' b) = 'pure' b
-- @
--
-- @since 1.0
evalRandom :: Applicative m => g -> RandomC g m a -> m a
evalRandom g = evalState g . runRandomC
{-# INLINE evalRandom #-}

-- | Run a random computation starting from a given generator and discarding the final result.
--
-- @
-- 'execRandom' g ('pure' b) = g
-- @
--
-- @since 1.0
execRandom :: Applicative m => g -> RandomC g m a -> m g
execRandom g = execState g . runRandomC
{-# INLINE execRandom #-}

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
--
-- @since 1.1
evalRandomSystem :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomSystem m = liftIO R.newStdGen >>= flip evalRandom m
{-# INLINE evalRandomSystem #-}

-- | A carrier for 'Random' implemented using 'R.RandomGen'.
--
-- @since 1.0
newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, R.RandomGen g) => Algebra (Random :+: sig) (RandomC g m) where
  alg hdl sig ctx = RandomC $ case sig of
    L random -> StateC $ \ k g -> case random of
      Uniform      -> let (a,   g') = R.random    g in k g' (a <$ ctx)
      UniformR r   -> let (a,   g') = R.randomR r g in k g' (a <$ ctx)
      Interleave m -> let (g'', g') = R.split     g in runState (const (k g'')) g' (runRandomC (hdl (m <$ ctx)))
    R other  -> alg (runRandomC . hdl) (R other) ctx
  {-# INLINE alg #-}
