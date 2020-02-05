{-# LANGUAGE Safe, MultiParamTypeClasses, FlexibleInstances  #-}

{-|

Module      : MonadReferencePlus
Description : Extending the application scope of monad references
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

-}


module MonadReferencePlus where

import MonadReference
import MonadStream
import MonadStreamReference


import Control.Concurrent.MVar  (newEmptyMVar, putMVar, takeMVar)

import Control.Concurrent(forkIO)
    
-- * Multiple concurrent read of references

-- | Concurrent reading of traversable structures of monad references 
class MonadRef m => MonadRefAndStream  m where
    readAllRef :: (Traversable t) => t(MRef m a) -> m (Stream m a)
    forkAllToStream :: (Traversable t) => t(m a) -> m (Stream m a)
    forkAllToStream l = (mapM forkToRef l) >>= readAllRef

instance MonadRefAndStream IO where
    readAllRef l  =  do
      -- a shared pipe  
      v <- newEmptyMVar     
      -- forks all given actions and pipes their returned values
      mapM_ forkIO (fmap (\r -> readRef r >>= putMVar v) l)
      return (streamNFromMVar (length l) v)
        where
          streamNFromMVar 0 _ = Stream (return Nothing)
          streamNFromMVar n v = Stream $ do
             a <- takeMVar v
             return $ Just (a, streamNFromMVar (n-1) v)
                                          
-- * Monadic structures references


-- | Structures of monad references
class MonadRef m => MonadDataRef t m where
    forkT :: t m a -> m (t (MRef m) a)
    readT :: t (MRef m) a -> m (t m a)
    -- tryReadT :: t (MRef m) a -> m (Maybe (t m a) , Maybe (t (MRef m) a))
    parReadT :: t (MRef m) a -> t (MRef m) b -> t m (Either a b)
    readAllT :: (Traversable c) => c (MRef m a) -> t m a            
                

instance MonadRefAndStream  m => MonadDataRef Stream m where
    forkT = forkStreamToRef
    readT = return . readStreamRef
    -- tryReadT = tryReadStreamRef
    parReadT = parReadStreamRef
    readAllT = toStream . readAllRef            


-- * Application to on-the-fly concurrent folding


-- | folds the values returned by a traversable structure of monad actions, these values ordered by termination time.
foldOnTime :: (MonadRefAndStream  m, Traversable t) =>
 (b -> a -> m b) -> b -> t (m a) -> m b
foldOnTime f b c =
  forkAllToStream c >>= foldMStream f b
                    
               
