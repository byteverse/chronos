{-# LANGUAGE CPP #-}

module Chronos.Internal.CTimespec
  ( getPosixNanoseconds
  ) where

import Foreign
import Foreign.C

#ifdef ghcjs_HOST_OS

-- Fix this at some point.
getPosixNanoseconds :: IO Int64
getPosixNanoseconds = return 0

#else

data CTimespec = CTimespec
  { ctimespecSeconds :: {-# UNPACK #-} !CTime
  , ctimespecNanoseconds :: {-# UNPACK #-} !CLong
  }

instance Storable CTimespec where
    sizeOf _ = (16)
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
        ns <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
        return (CTimespec s ns)
    poke p (CTimespec s ns) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p s
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p ns

foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt

-- | Get the current POSIX time from the system clock.
getPosixNanoseconds :: IO Int64
getPosixNanoseconds = do
  CTimespec (CTime s) (CLong ns) <- alloca $ \ptspec -> do
    throwErrnoIfMinus1_ "clock_gettime" $
      clock_gettime 0 ptspec
    peek ptspec
  return ((s * 1000000000) + ns)

#endif



