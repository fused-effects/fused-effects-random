{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Random
( -- * Random effect
  Random(..)
, getRandom
, getRandomR
, interleave
  -- * Random carrier
, runRandom
, evalRandom
, execRandom
, evalRandomIO
, RandomC(..)
  -- * Re-exports
, Has
, run
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Carrier.State.Strict
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import qualified Control.Monad.Random.Class as R
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class
import qualified System.Random as R (Random(..), RandomGen(..), StdGen, newStdGen)

data Random m k
  = forall a . R.Random a => Random (a -> m k)
  | forall a . R.Random a => RandomR (a, a) (a -> m k)
  | forall a . Interleave (m a) (a -> m k)

deriving instance Functor m => Functor (Random m)

instance HFunctor Random where
  hmap f (Random       k) = Random           (f . k)
  hmap f (RandomR r    k) = RandomR r        (f . k)
  hmap f (Interleave m k) = Interleave (f m) (f . k)
  {-# INLINE hmap #-}

instance Effect Random where
  thread state handler (Random       k) = Random                            (handler . (<$ state) . k)
  thread state handler (RandomR r    k) = RandomR r                         (handler . (<$ state) . k)
  thread state handler (Interleave m k) = Interleave (handler (m <$ state)) (handler . fmap k)
  {-# INLINE thread #-}


getRandom :: (Has Random sig m, R.Random a) => m a
getRandom = send (Random pure)

getRandomR :: (Has Random sig m, R.Random a) => (a, a) -> m a
getRandomR interval = send (RandomR interval pure)

interleave :: (Has Random sig m) => m a -> m a
interleave m = send (Interleave m pure)


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
evalRandomIO :: MonadIO m => RandomC R.StdGen m a -> m a
evalRandomIO m = liftIO R.newStdGen >>= flip evalRandom m

newtype RandomC g m a = RandomC { runRandomC :: StateC g m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Algebra sig m, Effect sig, R.RandomGen g) => R.MonadRandom (RandomC g m) where
  getRandom = getRandom
  {-# INLINE getRandom #-}
  getRandomR = getRandomR
  {-# INLINE getRandomR #-}
  getRandomRs interval = (:) <$> R.getRandomR interval <*> R.getRandomRs interval
  {-# INLINE getRandomRs #-}
  getRandoms = (:) <$> R.getRandom <*> R.getRandoms
  {-# INLINE getRandoms #-}

instance (Algebra sig m, Effect sig, R.RandomGen g) => R.MonadInterleave (RandomC g m) where
  interleave = interleave
  {-# INLINE interleave #-}

instance (Algebra sig m, Effect sig, R.RandomGen g) => Algebra (Random :+: sig) (RandomC g m) where
  alg (L (Random       k)) = RandomC $ do
    (a, g') <- gets R.random
    put (g' :: g)
    runRandomC (k a)
  alg (L (RandomR r    k)) = RandomC $ do
    (a, g') <- gets (R.randomR r)
    put (g' :: g)
    runRandomC (k a)
  alg (L (Interleave m k)) = RandomC $ do
    (g1, g2) <- gets R.split
    put (g1 :: g)
    a <- runRandomC m
    put g2
    runRandomC (k a)
  alg (R other)            = RandomC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import System.Random
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Effect.NonDet
-- >>> newtype PureGen = PureGen Int deriving (Eq, Show)
-- >>> instance RandomGen PureGen where next (PureGen i) = (i, PureGen i) ; split g = (g, g)
