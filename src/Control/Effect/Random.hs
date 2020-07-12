{-# LANGUAGE GADTs #-}
-- | Random variables in uniform and exponential distributions, with interleaving.
--
-- @since 1.0
module Control.Effect.Random
( -- * Random effect
  Random(..)
, uniform
, uniformR
, interleave
  -- * Non-uniform distributions
, exponential
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Algebra
import qualified System.Random as R (Random(..))

data Random m k where
  Uniform    :: R.Random a =>           Random m a
  UniformR   :: R.Random a => (a, a) -> Random m a
  Interleave :: m a                  -> Random m a


-- | Produce a random variable uniformly distributed in a range determined by its type’s 'R.Random' instance. For example:
--
-- * bounded types (instances of 'Bounded', such as 'Char') typically sample all of the constructors.
-- * fractional types, the range is normally the semi-closed interval [0,1).
-- * for 'Integer', the range is (arbitrarily) the range of 'Int'.
uniform :: (R.Random a, Has Random sig m) => m a
uniform = send Uniform
{-# INLINE uniform #-}

-- | Produce a random variable uniformly distributed in the given range.
--
-- @
-- 'Data.Ix.inRange' (a, b) '<$>' 'uniformR' (a, b) = 'pure' 'True'
-- @
uniformR :: (R.Random a, Has Random sig m) => (a, a) -> m a
uniformR interval = send (UniformR interval)
{-# INLINE uniformR #-}

-- | Run a computation by splitting the generator, using one half for the passed computation and the other for the continuation.
--
-- @
-- 'interleave' ('pure' a) = 'pure' a
-- @
interleave :: Has Random sig m => m a -> m a
interleave m = send (Interleave m)
{-# INLINE interleave #-}


-- * Non-uniform distributions

-- | Produce a random variable in an expnoential distribution with the given scale.
exponential :: (R.Random a, Floating a, Has Random sig m) => a -> m a
exponential a = do
  x <- uniform
  pure $! -log x / a
{-# INLINE exponential #-}
