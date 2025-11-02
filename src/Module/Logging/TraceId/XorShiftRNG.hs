-- | This module provides an implementation of a XorShift random number generator (RNG)
-- and a monad transformer for using it in computations.
--
-- It is lightweight and fast.
module Module.Logging.TraceId.XorShiftRNG
  ( -- * RNG
    RNG, newRNG, uniformDoubleFromRNG, uniformWord64FromRNG
    -- * RNGT
  , newWord64IO
    -- * MonadUniformValue
  , Word64, nextWord64, word64ToDouble, splitSeeds, mix64
  ) where

import Foreign
import System.Clock
import Control.Monad.IO.Class

import GHC.Exts (RealWorld)
import qualified Data.Primitive.PrimArray as P


-- | The RNG state, which is a foreign pointer to a 64-bit unsigned integer.
newtype RNG = RNG (P.MutablePrimArray RealWorld Word64)
--(ForeignPtr Word64)

-- | Working with seeds
nextWord64 :: Word64 -> Word64
nextWord64 !x0 =
  let !x1 = x0 `xor` (x0 `shiftL` 13)
      !x2 = x1 `xor` (x1 `shiftR` 7)
  in  (x2 `xor`) $! (x2 `shiftL` 17)
{-# INLINE nextWord64 #-}

-- SplitMix64 "avalanche" ------------------------------------------------------
mix64 :: Word64 -> Word64
mix64 !z0 =
  let !z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in  z2 `xor` (z2 `shiftR` 31)
{-# INLINE mix64 #-}

-- ----------------------------------------------------------------------------- 
-- | Turn one seed into @n@ independent seeds suitable for different threads.
--
--   *   Deterministic: same parent → same list.
--   *   Fast:   O(n), two multiplies + a few shifts per child.
--   *   Guarantees no child gets 0 (bad for XorShift).
--
splitSeeds :: [a] -> Word64 -> [(a, Word64)]
splitSeeds n parent =
  [ (a, forceNonZero . mix64 $ parent + fromIntegral i * 0x9e3779b97f4a7c15)
  | (i, a) <- zip [0::Int ..] n
  ]
  where
    -- XorShift fails when state == 0 or 0x8000000000000000.
    forceNonZero 0 = 0x6a09e667f3bcc908  -- arbitrary non-zero constant
    forceNonZero w = w
{-# INLINE splitSeeds #-}

newWord64IO :: MonadIO m => m Word64
newWord64IO = do
  TimeSpec s n <- liftIO $ getTime Monotonic
  pure $! fromIntegral s `xor` fromIntegral n
{-# INLINE newWord64IO #-}

word64ToDouble :: Word64 -> Double
word64ToDouble !x =
  let !d = fromIntegral (x `shiftR` 11) * 1.1102230246251565e-16
      -- 1/2^53 = 1.110…e-16
  in d
{-# INLINE word64ToDouble #-}

-- | Create a new RNG instance seeded with the current time.
newRNG :: IO RNG
newRNG = do
  marr <- P.newPrimArray 1 -- Create a mutable array of size 1
  TimeSpec s n <- getTime Monotonic -- Get current time in seconds and nanoseconds
  let !seed = fromIntegral s `xor` fromIntegral n -- Combine seconds and nanoseconds
  P.writePrimArray marr 0 seed -- Write the seed into the array with 0 index
  pure $ RNG marr
{-# INLINE newRNG #-}

-- | Generate a uniform random double in the range [0, 1) using the XorShift algorithm.
-- updates the RNG state.
uniformDoubleFromRNG :: RNG -> IO Double
uniformDoubleFromRNG rng = do
  x3 <- uniformWord64FromRNG rng
  -- top 53 bits → [0,1)
  pure $! fromIntegral (x3 `shiftR` 11) * 1.1102230246251565e-16
{-# INLINE uniformDoubleFromRNG #-}

-- | Generate a uniform Word64 using the XorShift algorithm.
-- updates the RNG state.
uniformWord64FromRNG :: RNG -> IO Word64
uniformWord64FromRNG (RNG marr) = do
  !x0 <- P.readPrimArray marr 0 -- Read the current state
  let !x1 = x0 `xor` (x0 `shiftL` 13)
      !x2 = x1 `xor` (x1 `shiftR` 7)
      !x3 = x2 `xor` (x2 `shiftL` 17)
  P.writePrimArray marr 0 x3 -- Update the state
  pure $! x3
{-# INLINE uniformWord64FromRNG #-}
