{-|

Module      : SyncAsyncStream
Description : Specializing streams with either synchronous or asynchronous usage
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

-}

module SyncAsyncStream where

import MonadStream

data Sync m a = Sync a (a -> a -> a) (Stream m a)
    -- at every instant, returns a list of values at that instant 


-- syncMerge :: Monad m => Stream m [a] -> Stream m [a] -> Stream m [a]
syncMerge :: Monad m =>
     (a -> a -> a) -> Stream m a -> Stream m a -> Stream m a
syncMerge f (Stream m1) (Stream m2) = Stream $ do
  c1 <- m1
  c2 <- m2
  case (c1, c2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just (a , sc)) -> return (Just (a,sc))
    (Just (a , sc),Nothing) -> return (Just (a,sc))
    (Just (a1 , sc1),Just (a2 , sc2)) ->
      return (Just (f a1 a2, syncMerge f sc1 sc2))

syncBind :: Monad m => Stream m a
     -> (a -> (b, b -> b -> b, Stream m b)) -> Stream m b
             
syncBind (Stream m) g = Stream $ do
  c <- m
  case c of
    Nothing -> return Nothing
    Just (a,sc) -> do
      let (b , f, Stream ma) = g a
      ca <- ma
      case ca of
        Nothing -> return (Just (b , syncBind sc g))
        Just(b0,scb) -> return (Just (b0, syncMerge f scb (syncBind sc g)))
            

                        
    
{-
                   
syncZip :: Monad m => [Stream m [a]] -> Stream m [a]
syncZip [] = (Stream . return . Just) ([], Stream (return Nothing))
syncZip (s:ls) = syncMerge s (syncZip ls)
                 

syncBind :: Monad m => Stream m [a] -> (a -> Stream m [b]) -> Stream m [b]
syncBind (Stream m) f =
    Stream $ do
      c <- m
      case c of
        Nothing -> return Nothing
        Just (la , sc) -> do
           let Stream m1 = syncZip (fmap f la)
           c1 <- m1                
           case c1 of
             Nothing -> return (Just ([], syncBind sc f))
             Just (b,sc1) -> return (Just(b, syncMerge sc1 (syncBind sc f)))
                             
instance Monad m => Functor (Sync m) where
    fmap f (Sync s) = Sync (fmap (fmap f) s)


instance Monad m => Applicative (Sync m) where
    pure = return
    mf <*> m = mf >>= \f -> m >>= \a -> return (f a) 
                      
instance (Monad m) => Monad (Sync m) where
    return a = (Sync . Stream . return . Just) ([a],empty)
               where empty = (Stream . return) Nothing
    (Sync s) >>= f =
        let g a = let Sync s1  = f a in s1
        in Sync (syncBind s g)

-}
