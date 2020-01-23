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
--   prop> run (runRandom (PureGen a) (pure b)) === (PureGen a, b)
runRandom :: g -> RandomC g m a -> m (g, a)
runRandom g = runState g . runRandomC

-- | Run a random computation starting from a given generator and discarding the final generator.
--
--   prop> run (evalRandom (PureGen a) (pure b)) === b
evalRandom :: Functor m => g -> RandomC g m a -> m a
evalRandom g = fmap snd . runRandom g

-- | Run a random computation starting from a given generator and discarding the final result.
--
--   prop> run (execRandom (PureGen a) (pure b)) === PureGen a
execRandom :: Functor m => g -> RandomC g m a -> m g
execRandom g = fmap fst . runRandom g

-- | Run a random computation in 'IO', splitting the global standard generator to get a new one for the computation.
evalRandomSystem :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomSystem m = liftIO R.newStdGen >>= flip evalRandom m

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, Effect sig, R.RandomGen g) => Algebra (Random :+: sig) (RandomC g m) where
  alg = \case
    L (Random       k) -> RandomC $ do
      (a, g') <- gets R.random
      put (g' :: g)
      runRandomC (k a)
    L (RandomR r    k) -> RandomC $ do
      (a, g') <- gets (R.randomR r)
      put (g' :: g)
      runRandomC (k a)
    L (Interleave m k) -> RandomC $ do
      (g1, g2) <- gets R.split
      put (g1 :: g)
      a <- runRandomC m
      put g2
      runRandomC (k a)
    R other            -> RandomC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}
