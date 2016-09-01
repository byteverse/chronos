module Chronos.Internal.CTimespec where

import Foreign
import Foreign.C

data CTimespec = MkCTimespec CTime CLong

instance Storable CTimespec where
    sizeOf _ = (16)
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
        ns <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
        return (MkCTimespec s ns)
    poke p (MkCTimespec s ns) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p s
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p ns

foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt

-- | Get the current POSIX time from the system clock.
getCTimespec :: IO CTimespec
getCTimespec = alloca (\ptspec -> do
    throwErrnoIfMinus1_ "clock_gettime" $
        clock_gettime 0 ptspec
    peek ptspec
    )


