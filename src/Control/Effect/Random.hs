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


-- | Produce a random variable uniformly distributed in a range determined by its type’s 'R.Random' instance. For example:
--
-- * bounded types (instances of 'Bounded', such as 'Char') typically sample all of the constructors.
-- * fractional types, the range is normally the semi-closed interval [0,1).
-- * for 'Integer', the range is (arbitrarily) the range of 'Int'.
random :: (Has Random sig m, R.Random a) => m a
random = send (Random pure)

-- | Produce a random variable uniformly distributed in the given range.
--
-- @
-- 'Data.Ix.inRange' (a, b) '<$>' 'randomR' (a, b) = 'pure' 'True'
-- @
randomR :: (Has Random sig m, R.Random a) => (a, a) -> m a
randomR interval = send (RandomR interval pure)

-- | Run a computation by splitting the generator, using one half for the passed computation and the other for the continuation.
interleave :: (Has Random sig m) => m a -> m a
interleave m = send (Interleave m pure)
