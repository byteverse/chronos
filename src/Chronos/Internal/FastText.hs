{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

module Chronos.Internal.FastText where

import           Control.Monad.ST               (ST)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Array                as A
import qualified Data.Text.Internal             as I
import qualified Data.Text.Internal.Unsafe.Char as C

data MArrayAppend s = MArrayAppend (Int -> A.MArray s -> ST s Int) !Int

combine :: MArrayAppend s -> MArrayAppend s -> MArrayAppend s
combine (MArrayAppend f maxSizeA) (MArrayAppend g maxSizeB) = do
  MArrayAppend (\i marr -> f i marr >>= (\j -> g j marr)) (maxSizeA + maxSizeB)

runMArrayAppend :: (forall s. MArrayAppend s) -> Text
runMArrayAppend x =
  let (!arr, !word16Used) = A.run2 $ case x of
        MArrayAppend f maxSize -> do
          marr <- A.new maxSize
          finalSize <- f 0 marr
          return (marr,finalSize)
   in I.Text arr 0 word16Used

singleton :: Char -> MArrayAppend s
singleton c = MArrayAppend (\i marr -> do
  C.unsafeWrite marr i c
  ) 2

fromText :: Text -> MArrayAppend s
fromText (I.Text arr start len) = MArrayAppend (\i marr -> do
  let j = len + i
  A.copyI marr i arr start j
  return j ) len



