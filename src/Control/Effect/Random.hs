{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
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
, Effect
, Has
, run
) where

import           Control.Algebra
import qualified System.Random as R (Random(..))

data Random m k
  = forall a . R.Random a => Uniform (a -> m k)
  | forall a . R.Random a => UniformR (a, a) (a -> m k)
  | forall a . Interleave (m a) (a -> m k)

deriving instance Functor m => Functor (Random m)

instance Effect Random where
  thread state handler = \case
    Uniform      k -> Uniform                            (handler . (<$ state) . k)
    UniformR r   k -> UniformR r                         (handler . (<$ state) . k)
    Interleave m k -> Interleave (handler (m <$ state)) (handler . fmap k)
  {-# INLINE thread #-}


-- | Produce a random variable uniformly distributed in a range determined by its type’s 'R.Random' instance. For example:
--
-- * bounded types (instances of 'Bounded', such as 'Char') typically sample all of the constructors.
-- * fractional types, the range is normally the semi-closed interval [0,1).
-- * for 'Integer', the range is (arbitrarily) the range of 'Int'.
uniform :: (R.Random a, Has Random sig m) => m a
uniform = send (Uniform pure)
{-# INLINE uniform #-}

-- | Produce a random variable uniformly distributed in the given range.
--
-- @
-- 'Data.Ix.inRange' (a, b) '<$>' 'randomR' (a, b) = 'pure' 'True'
-- @
uniformR :: (R.Random a, Has Random sig m) => (a, a) -> m a
uniformR interval = send (UniformR interval pure)
{-# INLINE uniformR #-}

-- | Run a computation by splitting the generator, using one half for the passed computation and the other for the continuation.
--
-- @
-- 'interleave' ('pure' a) = 'pure' a
-- @
interleave :: Has Random sig m => m a -> m a
interleave m = send (Interleave m pure)
{-# INLINE interleave #-}


-- * Non-uniform distributions

-- | Produce a random variable in an expnoential distribution with the given scale.
exponential :: (R.Random a, Floating a, Has Random sig m) => a -> m a
exponential a = do
  x <- uniform
  pure $! -log x / a
{-# INLINE exponential #-}
