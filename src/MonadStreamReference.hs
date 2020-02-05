{-# LANGUAGE Safe,  TypeFamilies , BangPatterns, FlexibleInstances #-}


{-|

Module      : MonadStreamReference
Description : Fork and reading monad streams via monad references
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

Monad stream references, obtained by forking monad streams, actually
models by streams of monad references. The core of asynchronous stream
programing is made available thansk to the notion of monad stream
references.

With 'MonadReferences', therefore concurrency, one can define
asynchronous functions over streams, where input streams are red "as
fast" as possible, the effects of these values being interleaved.

A  function with several monad stream as inputs and one monad stream as
output that reads its input /when available/ and produces its output at will 
shall be called asynchronous.

The archetypal example of an asynchronous is the function
'mergeStream' that concurrently read two input streams as fast as
possible and produces the stream resulting from the interleaving of
these reading. We have:

/mergeStream :: MonadRef m => Stream m a -> Stream m b -> Stream m (Either a b)/.

In the category of asynchronous stream function, /Stream m (Either a b)/
is the product of /Stream m a/ and /Stream m b/ with mergeStream as
pairing function.

-}

module MonadStreamReference where

-- import Control.Monad.Identity    

import Control.Monad.Trans
    
import MonadReference
import MonadStream

    
import Control.Concurrent (forkIO, newEmptyMVar,takeMVar,putMVar)    

    

    
    
-- * Monad stream reference

-- | the monad stream reference type
type StreamRef m = Stream (MRef m)

-- | forks a monad stream and returns a monad stream ref
forkStreamToRef :: MonadRef m =>
  Stream m a -> m (StreamRef m a)
forkStreamToRef (Stream m) = do
  r <- forkToRef $ do
         c <- m
         case c of
           Nothing -> return Nothing
           Just(a,sc) -> do
             rc <- forkStreamToRef sc
             return $ Just(a, rc)
  (return . Stream) r                                    
{-         
    case c of
      Nothing -> return Nothing
      Just (!a, sc) -> do
        rc <- forkToRef (evalAndFork sc)
        return $ Just(a, Stream rc)
  -}

-- | turns a monad stream of references into a monad stream.
readStreamRef :: MonadRef m =>
  StreamRef m a -> Stream m a
readStreamRef (Stream v) = Stream $ readRef v >>=
  mapM (\(!a,rc) -> return (a, readStreamRef rc))

{-       
  c <- readRef v
  case c of
    Nothing -> return Nothing
    Just (!a,rc) -> return $ Just $ (a, readStreamRef rc)
-}

-- | merges two stream of references into one

parReadStreamRef :: MonadRef m => StreamRef m a -> StreamRef m b -> Stream m (Either a b)
parReadStreamRef sr1@(Stream r1) sr2@(Stream r2) = Stream $ do
  c <- parReadRef r1 r2
  case c of
    Left  Nothing -> next $ fmap Right $ readStreamRef (Stream r2)
    Right Nothing -> next $ fmap Left $ readStreamRef (Stream r1)
    Left  (Just (a , src1))
        -> return $ Just (Left a , parReadStreamRef src1 sr2)
    Right (Just (b , src2))
        -> return $ Just (Right b , parReadStreamRef sr1 src2)


                      
                    

               
-- * Involving safe strem copies
               
-- ** Duplicating

-- | duplicates a stream by forkeing it and returnining two copies of
-- reading actions.
duplicate :: MonadRef m => Stream m a -> m (Stream m a, Stream m a)
duplicate s = do
  r <- forkStreamToRef s
  return (readStreamRef r, readStreamRef r)

-- ** Unzipping

-- | unzips a monad stream into two monad streams, via a (free) copy
-- of the zipped stream, thanks to monad references.
unzipStream :: MonadRef m => Stream m (a,b) ->
    m (Stream m a , Stream m b)
unzipStream s = do
  r <- forkStreamToRef s
  return  (  fmap fst (readStreamRef r),
             fmap snd (readStreamRef r))

-- ** Spaning and co-spanning


-- | applies two synchronous functions over the same input stream
-- and synchronously merge their result. This function uses 'MonadRef'
-- and 'StreamRef' for duplicating the input stream.
spanStream :: MonadRef m => (b -> c -> d)
     -> (Stream m a -> Stream m b) -> (Stream m a -> Stream m c)
     -> Stream m a -> Stream m d
spanStream c f g s = toStream $ do
  r <- forkStreamToRef s
  let si = readStreamRef r
  return $ zipStreamWith c (f si) (g si)

             
-- ** Loops with asynchronous control

-- | A state based synchronous stream function with asynchronous
-- control/update of state based step function. See 'Audio.Audio' with
-- 'Audio.BufferState' example for an efficient (constant time update)
-- state space.  The control stream is forked and
-- 'loopStreamWithControlRef' is called, so that reading control values
-- makes no interferences with any other possible reading of the same
-- control stream, i.e. it is duplicated.
loopStreamWithControl :: MonadRef m => s -> ((a,s) -> m (b,s))
   -> Stream m ((a,s) -> m (b,s)) -> Stream m a -> Stream m b
loopStreamWithControl s f sf sa = toStream $ do
  r <- forkStreamToRef sf
  return $ loopStreamWithControlRef s f r sa     

-- | A state based synchronous stream function with asynchronous
-- control/update of state based step function. The control stream
-- argument is a monad stream reference.
loopStreamWithControlRef :: MonadRef m => s -> ((a,s) -> m (b,s))
   -> StreamRef m ((a,s) -> m (b,s)) -> Stream m a -> Stream m b      
loopStreamWithControlRef s f sf@(Stream msf) (Stream m) = Stream $ do
  cg <- tryReadRef msf
  let (f1,sf1) = case cg of
        Nothing -> (f,sf) -- no new control
        Just Nothing -> (f,sf)  -- control stream terminated
        Just (Just(g,sfc)) -> (g,sfc) -- new control
  c <- m
  case c of
    Nothing -> return Nothing
          -- end of input stream
    Just (a , sc) -> do
      (b,s1) <- f1 (a,s)
      return $ Just (b , loopStreamWithControlRef s1 f1 sf1 sc)

             

    
-- ** Synchronous application of asynchronously changing maps

-- | the first arguement is the inital map, the second the
-- (aynchronous) stream of other maps
streamMap :: MonadRef m => (a -> m b) ->
  Stream m (a -> m b) -> Stream m a -> Stream m b
streamMap f sf s = toStream $ do
  r <- forkStreamToRef sf
  return $ streamMapRef f r s

-- | same as above but from a stream ref of functions               
streamMapRef :: MonadRef m => (a -> m b) ->
  StreamRef m (a -> m b) -> Stream m a -> Stream m b
streamMapRef f sf@(Stream r) (Stream m) = Stream $ do
  cf <- tryReadRef r
  let (f1, sf1) = case cf of
         Just (Just(f1,sf1)) -> (f1,sf1)
         _ -> (f,sf)
  c <- m
  case c of
    Nothing -> return Nothing
    Just(a,sc) -> do
           b <- f1 a
           return $ Just(b,streamMapRef f1 sf1 sc)
    
-- | An application of the above (just receiving an asynchronous stream of parameter)
streamMapWithParameter :: MonadRef m =>
    (p -> a -> m b) -> p -> Stream m p -> Stream m a -> Stream m b            
streamMapWithParameter f p sp s
   = streamMap (f p) (fmap f  sp)  s
      
-- ** racing (first version)

-- | merging streams as above but stopping the merge as soon as one is terminated. 
raceStream :: MonadRef m => Stream m a -> Stream m a -> Stream m a
raceStream (Stream m1) (Stream m2) = Stream $ do
  r1 <- forkToRef m1
  r2 <- forkToRef m2
  c <- parReadRef r1 r2
  case c of
    Left  Nothing -> return Nothing
    Right Nothing -> return Nothing
    Left  (Just (a , mc1)) -> return $
      Just (a , raceStream mc1 (Stream $ readRef r2))
    Right (Just (a , mc2)) -> return $
      Just (a , raceStream (Stream $ readRef r1) mc2)

           
-- | merging streams refs as above but stopping the merge as soon as
-- one of the referenced stream is terminated.
raceStreamRef :: MonadRef m => StreamRef m a -> StreamRef m a -> m(StreamRef m a)
raceStreamRef r1 r2 = forkStreamToRef (raceStream (readStreamRef r1) (readStreamRef r2))



-- * Vertical monoid structure
         
-- ** Merge

-- | merges two monad streams into a single ordering returned values by action
-- termination time. The corner stone of the 'Applicative' and 'Monad'
-- instances of monad streams.
merge :: MonadRef m =>
  Stream m a -> Stream m a -> Stream m a                  
merge s1 s2  = toStream $ do
  r1 <- forkStreamToRef s1
  r2 <- forkStreamToRef s2
  return $ mergeRef r1 r2      
  

-- | merges two referenced monad streams into a single monad stream,
-- by combining elements by order of availability.
mergeRef :: MonadRef m => StreamRef m a -> StreamRef m a -> Stream m a    
mergeRef sr1@(Stream r1) sr2@(Stream r2) = Stream $ do
  -- this code is written in such a way that there are at most two pending monad references at any instant,
  c <- parReadRef r1 r2
  case c of
    Left  Nothing -> next $ readStreamRef (Stream r2)
    Right Nothing -> next $ readStreamRef (Stream r1)
    Left  (Just (a , src1)) -> return $
      Just (a , mergeRef src1 sr2)
    Right (Just (a , src2)) -> return $
      Just (a , mergeRef sr1 src2)

-- | Vertical stream semigroup
instance MonadRef m => Semigroup (Stream m a) where
  (<>) = merge
         
-- | Horizontal stream monoid
instance MonadRef m => Monoid (Stream m a) where
    mempty = emptyStream



-- | forks all the monad action in a traversable structure             
forkAll :: (Traversable t , MonadRef m) => t (m a) -> m (t (MRef m a))
forkAll = mapM forkToRef

-- | turns a traversable structure of monad references into a monad
-- stream which elements are the values returned by the referenced
-- actions ordered by termination time.
readAll :: (Traversable t, MonadRef m) => t (MRef m a) -> Stream m a
readAll =  foldMap (lift . readRef)
             

-- | more efficient version of readAll for IO references.
readAllIO :: Traversable t => t (MRef IO a) -> IO (Stream IO a)
readAllIO t = do {v <- newEmptyMVar;
    mapM_ (\r -> forkIO (readRef r >>= putMVar v)) t;
    return $ mvarToStream v (length t)}
  where
    mvarToStream _ 0 = mempty
    mvarToStream v n = Stream $ do {a <- takeMVar v; 
            return $ Just(a,  mvarToStream v (n -1))}

            
instance MonadRef m => Applicative (Stream m) where
    pure = return
    (<*>) = applyStream

-- | Maps a changing function (get out from a stream) to a stream of
-- argument
applyStream :: MonadRef m => Stream m (a -> b) -> Stream m a -> Stream m b               
applyStream sf  s = toStream $ do
    rf <- forkStreamToRef sf
    r <- forkStreamToRef s
    return $ applyStreamRef rf r

-- | Maps a changing function (get out from a stream reference) to a
-- stream reference of argument
applyStreamRef :: MonadRef m => StreamRef m (a -> b) -> StreamRef m a -> Stream m b                   
applyStreamRef (Stream rf) (Stream r) = toStream $ do
    c <- parReadRef rf r
    case c of
      Left Nothing -> return mempty
      Left (Just(f,rfc)) -> return $
          applyStreamRefWith f rfc (Stream r)
      Right Nothing ->  return mempty
      Right (Just(_,rc)) -> return $ applyStreamRef (Stream rf) rc

-- | Maps a changing function (get out from a stream reference) to a
-- stream reference of argument, the first function being given
applyStreamRefWith :: MonadRef m => (a -> b) -> StreamRef m (a -> b) -> StreamRef m a -> Stream m b                             
applyStreamRefWith f (Stream rf) (Stream r) = Stream $ do
    c <- parReadRef rf r
    case c of
      Left Nothing -> return Nothing
      Left (Just(g,rfc)) -> let (Stream m) = applyStreamRefWith g rfc (Stream r) in m
      Right Nothing ->  return Nothing
      Right (Just(a,rc)) -> return $ Just(f a, applyStreamRefWith f (Stream rf) rc)
                                              
                                              
                     
instance MonadRef m => Monad (Stream m) where
  return a = (Stream . return . Just) (a, mempty)
  (>>=) (Stream m) f = Stream $ do
    c <- m
    case c of
      Nothing -> return Nothing
      Just (a,mc) ->
        next $ merge (f a) (mc >>= f)

             

-- | Need MonadRef m for Stream m to be a monad
instance MonadIO (Stream IO) where
    liftIO = lift
                      
instance MonadRef m => MonadRefCore (Stream m) where
    type MRef (Stream m)  = StreamRef m
    forkToRef (Stream m) = Stream $ do
      r <- forkToRef (toStreamRef m)
      return $ Just (Stream r,mempty)
        where
      toStreamRef :: MonadRef m => m (Maybe (a , Stream m a)) -> m (Maybe (a , StreamRef m a)) 
      toStreamRef m =  do
        c <- m
        case c of
          Nothing -> return Nothing
          Just(a,(Stream mc)) -> do
            vc <- forkToRef (toStreamRef mc)
            return $ Just(a,Stream vc)
    readRef (Stream v) = Stream $ do
      c <- readRef v
      case c of
        Nothing -> return Nothing
        Just(a,r) -> return $ Just(a, readRef r)
    
    
instance MonadRef m => MonadRef (Stream m) where
    tryReadRef (Stream v) =  Stream $ do
      c <- tryReadRef v
      case c of
        Nothing -> return Nothing
        Just Nothing -> return $ Just (Nothing, mempty)
        Just (Just (a,r)) -> return $ Just (Just a , tryReadRef r)
    parReadRef r1 r2 =
      merge (fmap Left (readRef r1)) (fmap Right (readRef r2))

-- * Asynchronous categorical product

-- ** Heterogeneous merging
-- | merges two monad streams of different element type into one.
mergeStream :: MonadRef m => Stream m a -> Stream m b -> Stream m (Either a b)
mergeStream s1 s2 = merge (fmap Left s1) (fmap Right s2)

-- ** Projections              

-- | first projection of a merged stream.
fstStream :: MonadRef m => Stream m (Either a b) -> Stream m a
fstStream s = filterMaybe fromLeft s
  where fromLeft (Left a) = Just a
        fromLeft _ = Nothing
-- | snd projection of a merged stream.
--
-- It occurs that @Stream m (Either a b)@ with 'fstStream' and
-- 'sndStream' projections defined above is (almost) the categorical
-- product of @Stream m a@ and @Stream m b@ in the category of, say,
-- synchronous functions between streams.
--
-- Warning : we say /almost/ because getting both the fst and second
-- projections as defined above necessitates to run twice the merged
-- stream.  The following 'splitH' function avoids such a
-- drawback, but at the cost of embedding the two projections into a
-- single monad action.
sndStream :: MonadRef m => Stream m (Either a b) -> Stream m b
sndStream s = filterMaybe fromRight s
  where fromRight (Right b) = Just b
        fromLeft _ = Nothing

-- ** Splitting

-- | splits a merged stream into two streams, running only /once/ the
-- merged stream. This is made possible thnaks to monad stream
-- references.
splitH :: MonadRef m => Stream m (Either a b) -> m (Stream m a , Stream m b)
splitH s = do
  r <- forkStreamToRef s
  return (fstStream (readStreamRef r) ,  sndStream (readStreamRef r))


         

-- * some tests

-- | echoes the standard input to the standard output as above but via stream forking.
echoStdIOviaRef :: IO()
echoStdIOviaRef = do 
  r <- forkStreamToRef stdinStream
  streamStdout (readStreamRef r)


-- | double echoes the standard input to the standard output. Run fast with NO MEMORY LEAK !
echo2StdIO :: IO()
echo2StdIO
    = stream2Stdout (toStream $ do
                      r <- forkStreamToRef stdinStream
                      let s = zipStream (readStreamRef r) (readStreamRef r)
                      return s)


-- | double echoes the standard input to the standard output. Run fast with NO MEMORY LEAK !
echoStdIORef :: IO()
echoStdIORef
    = streamStdout (toStream $ do
                      r <- forkStreamToRef stdinStream
                      return (readStreamRef r))
                                
