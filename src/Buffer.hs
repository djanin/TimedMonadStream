{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

{-|
Module      : Buffer
Description : Bounded (but unchecked) cyclic buffer with fifo behavior
Copyright   : (c) David Janin 2019
License     : see the LICENSE file in the distribution
Maintainer  : janin@labri.fr
Stability   : experimental

Bounded cyclic buffer, allocated once for all with a maximulm size and
later use as an efficient (bounded) FIFO. As a mater of fact, state
argument in realtime-loop shall be of constant size in order to avoid
any memory allocation.

-}



module Buffer where

import qualified Data.Vector.Storable.Mutable as VS
import Data.IORef
import Foreign.Storable
import Foreign.Ptr

-- import System.IO.Unsafe(unsafePerformIO)

-- * Header indices describing a cyclic buffer

-- | A bound on the size of a buffer
maxSize :: Int
maxSize = 2^18


-- | A adhoc circular (FIFO) buffer         
data Buffer v = Buffer 
    {din  :: IORef Int,
     dout :: IORef Int,
     content :: VS.IOVector v
    }

-- | Returns an empty buffer
newEmptyBuffer ::  (Storable v) => IO (Buffer v)
newEmptyBuffer =
    do
      din <- newIORef 0
      dout <- newIORef 0
      content <- VS.new maxSize
      return (Buffer din dout content)

-- | Gets one element from the FIFO. The function does not check
-- for emptyness, and, in this case, returns whatever was there
-- before.
readBuffer :: Storable v => Buffer v -> IO v
readBuffer   (Buffer _ dout content) =
    do
      i <- readIORef dout
      modifyIORef' dout (\x -> mod (x+1) maxSize)
      VS.read content i

        
-- | Puts one element in the FIFO. The function does not check
-- for max sized reached, smashing former values in this case.
writeBuffer :: Storable v => Buffer v -> v -> IO ()           
writeBuffer (Buffer din _ content) v =
    do
      i <- readIORef din
      modifyIORef' din (\x -> mod (x+1) maxSize)
      VS.write content i v

-- | Puts a list of element into a buffer. This can freely be used at
-- init time. But list should be avoided at loop time.
writeList2Buffer :: Storable v => Buffer v -> [v] -> IO ()
writeList2Buffer b l = putList2Buffer b l where
    putList2Buffer _ [] = return ()
    putList2Buffer b (v:l) = writeBuffer b v >> putList2Buffer b l                     

-- | Flush the buffer content into a list. Could be used for debug ?
getBufferContents :: (Eq v , Storable v) => Buffer v -> IO [v]
getBufferContents b@(Buffer din dout _) = do
  o <- readIORef dout
  i <- readIORef din
  case (o == i) of
    True -> return []
    False -> do
      v <- readBuffer b
      l <- getBufferContents b
      return (v:l)
      
-- | Stereo samples are also storable
instance Storable v => Storable (v,v) where
    sizeOf _ = 2*sizeOf (undefined::v)
    {-# INLINE sizeOf #-}
    alignment _ = alignment (undefined::v)
    {-# INLINE alignment #-}
    poke ptr (a,b) =
        do mapM_ (\(n,x) -> pokeElemOff ptr' n x) $ [(0,a),(1,b)]
            where ptr' = castPtr ptr
    {-# INLINE poke #-}
    peek ptr = do
      a <- peekElemOff ptr' 0 :: IO v
      b <- peekElemOff ptr' 1 :: IO v
      return (a,b)       
        where ptr' = castPtr ptr
    {-# INLINE peek #-}
