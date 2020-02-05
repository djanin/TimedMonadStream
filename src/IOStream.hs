{-# LANGUAGE Safe, BangPatterns #-}

{-|

Module      : IOStream
Description : Monad streams when applied to the IO monad
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

Some more specialized function for monad stream when instanciated over the IO Monad. 
-}


module IOStream where

import Control.Concurrent.Chan

import MonadStream (Stream(..), toStream)

import Control.Concurrent (forkIO)    
         
-- ** Streams and channels
         
-- | makes an infinite stream from an (input) Channel
makeStreamFromChan :: Chan a -> Stream IO a
makeStreamFromChan ch = Stream $ do
  a <- readChan ch
  return $  Just (a, makeStreamFromChan ch)
         
-- | makes an (output) channel from a stream, initialiazed by a finite constant streaming.
-- Warning : no termination markers in the output channel.
makeChanFromStream :: Int -> a -> Stream IO a -> IO(Chan a) 
makeChanFromStream n ia s = do
  ch <- newChan
  writeList2Chan ch (take n (repeat ia))
  _ <- forkIO$ dropStreamToChan ch s
  return ch
    where
  dropStreamToChan ch (Stream m) = do
    c <- m
    case c of
      Nothing -> return ()
      Just(a,mc) -> seq a  $ (writeChan ch a) >> dropStreamToChan ch mc
                    
-- ** More synchronous monad stream related function within the Monad 


-- | returns the monad map associated to a stream map with specified delay length 
streamToMapN :: Int -> b ->
  (Stream IO a -> Stream IO b) -> IO (a -> IO b)                  
streamToMapN n b f = do
  inChan <- newChan
  outChan <- makeChanFromStream n b $
             f (makeStreamFromChan inChan)
  return (\a -> seq a $ writeChan inChan a
                >> readChan outChan)         

-- | same as above with zero delay (much less efficient as it forces many synchronizations)
streamToMap :: (Stream IO a -> Stream IO b) -> IO (a -> IO b)                  
streamToMap f =  streamToMapN 0 undef f
                 where undef = error "streamToMap : such a value shall never be needed"
  
        
-- | More efficient (?) version of merge stream in the IO monad,
-- solving race by concurrent acces to an IO chan.
mergeStreamIO :: Stream IO a -> Stream IO b -> Stream IO (Either a b)
mergeStreamIO sa sb = toStream $ do
  ch <- newChan
  _ <- forkIO $ dropToChan ch Right sb
  _ <- forkIO $ dropToChan ch Left sa
  return $ makeStreamFromChan ch
    where
      dropToChan ch f (Stream m) = do
        c <- m
        case c of
          Just (v,s) -> do     
            writeChan ch (f v)
            dropToChan ch f s
          Nothing -> return ()
       
    
