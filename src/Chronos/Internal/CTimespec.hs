{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

module Chronos.Internal.CTimespec
  ( getPosixNanoseconds
  ) where

import Foreign
import Foreign.C

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "Date.now()" currentSeconds :: IO Double
getPosixNanoseconds :: IO Int64
getPosixNanoseconds = do 
  x <- currentSeconds 
  pure $ fromIntegral $ 1000000 * (round x)
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

#ifdef darwin_HOST_OS
foreign import ccall unsafe "cbits/hs-time.c clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt
#else
foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt
#endif

-- | Get the current POSIX time from the system clock.
getPosixNanoseconds :: IO Int64
getPosixNanoseconds = do
  CTimespec (CTime s) (CLong ns) <- alloca $ \ptspec -> do
    throwErrnoIfMinus1_ "clock_gettime" $
      clock_gettime 0 ptspec
    peek ptspec
  return ((s * 1000000000) + ns)

#endif
