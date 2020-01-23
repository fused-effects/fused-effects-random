{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Random
( -- * Random effect
  Random(..)
, random
, randomR
, interleave
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import           Control.Algebra
import qualified System.Random as R (Random(..))

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


random :: (Has Random sig m, R.Random a) => m a
random = send (Random pure)

-- | Produce a random variable uniformly distributed in the given range.
randomR :: (Has Random sig m, R.Random a) => (a, a) -> m a
randomR interval = send (RandomR interval pure)

interleave :: (Has Random sig m) => m a -> m a
interleave m = send (Interleave m pure)
