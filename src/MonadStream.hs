{-# LANGUAGE Safe, TypeFamilies, FlexibleInstances, BangPatterns #-}

{-|

Module      : MonadStream
Description : Finite or infinite streams defined by nested moandic actions
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

A monad stream is simply the (non deprecated) monad list transformer
applied to some monad.  For the sake of readability, all definitions
are made anew. This notion is already extensively implemented in
Haskell under many variants including 'Data.Conduit' library by
Michael Snoyman.  


The emphasis proposed here is more on Control than on Data, departing from the
standard view of stream as (generalized) lists. Generally, one don't
want to concatenate two input monad stream. Zipping them is possible,
provided both stream can be produced at the same frequency.

By essence, a monad stream is simply a monad action that either return
nothing (the stream is over) or just the next value of the stream and
the monad action of its continuation (the stream goes on).

With a view towards application, monad streams allow for encoding
(even in non lazy language) lazy lists, but with some flavor of
sequentiality and a stronger control on the way 
these lists are red and/or defined.  

A good point with monad streams: they encode both input or output streams.
As far as intuition is concerned:

* as input : one can read a monad stream by (inductively) "executing"
the monad action it contains,

* as ouput : one can write a monad stream by (inductively) "defining"
the monad action it contains.

When defining a function over streams, one therefore has full control
on the synchronicity with which inputs are red and output are produced.


WARNING : /A priori/, a monad stream /SHOULD NOT/ be duplicated. Every
action has a side effect. Using two copies of the same monad stream
does not duplicate the effects, but, instead, distibutes them. For
instance, reading concurrently two instance of 'stdinStream' actually
distributes the inputs among these two readings. 

/A Posteriori/, a
stream can be /forked/ by 'MonadStreamReference.forkStreamToRef' and the resulting promise
of a stream can be /freely read/ by 'MonadStreamReference.readStreamRef' as defined in 'MonadStreamReference.MonadStreamReference' when extending the notion of monad stream to the notion of monad stream references. 


-}

--  Remark : following Conal Elliot Push-Pull paper with FRP terminology :
--  * monad stream are Event,
--  * pairs of (current) value and a (continuation) monad stream are Reactive


module MonadStream where

import Control.Monad.Identity    

import Control.Monad.Trans
    
import System.IO (hIsEOF, stdin)

import MonadReference
    
-- import Control.Monad.IO.Class()
    
-- * Monad streams
    
-- | quite a generic definition of finite or infinite streams
-- parameterized by a type functor @f : * -> *@ that tells how
-- one element may be connected (if ever) with the next one.
--
-- For instance, the type @Stream Identity a@ is essentially isomorphic
-- to the type @[a]@.
newtype  Stream f a = Stream {next :: f (Maybe (a , Stream f a))}


instance (Show a) => Show (Stream Identity a) where
    show (Stream (Identity c)) =
      case c of
        Nothing -> "Stream (return Nothing)"
        Just (a , mc) -> "Stream (return (Just (" ++ show a ++ "," ++ show mc ++ ")))"


instance (Show a) => Show (Stream (Stream Identity) a) where
    show (Stream (Stream s)) =
       "Stream (Stream (" ++ show s ++ "))"

instance Functor m => Functor (Stream m) where
    fmap f (Stream m) = Stream $ fmap (fmap (\(a, sc) -> (f a, fmap f sc))) m

                        
-- ** Standard input and output and streams


-- | turns the standard input into an IO stream. This SHOULD NOT be
-- duplicated unless one truly wishes to distribute inputs among
-- several readers.
stdinStream :: Stream IO Char
stdinStream  = Stream $ do
  c <- hIsEOF stdin
  if c then return Nothing
  else do 
    a <- getChar
    return $ Just (a, stdinStream)

-- | outputs stream argument to sdtout                 
streamStdout :: Stream IO Char -> IO ()
streamStdout (Stream m) = do
  c <- m
  case c of
    Nothing -> return ()
    Just(a,s) -> do
      putChar a
      streamStdout s

-- | echoes the standard input to the standard output. Run fast with NO MEMORY LEAK !
echoStdIO :: IO()
echoStdIO = streamStdout stdinStream
             
-- ** Running or folding streams

             
{-
-- | turns a monad stream into a monad action that runs all embeded
-- actions eventually returning its last vakue if ever
runStream :: Monad m => Stream m a -> m (Maybe a)
runStream = foldStream (const $ \y -> (return . Just) y) Nothing
-}

-- | ignores the content of a stream
ignoreStream :: Monad m => Stream m a -> Stream m ()
ignoreStream = fmap (const ())

-- | runs all the action of a stream of units
execStream :: Monad m => Stream m () -> m ()
execStream (Stream m) = do
  c <- m
  case c of
    Nothing -> return ()
    Just(_,sc) -> execStream sc
                  

-- | folds a monad stream. Allthough the type of 'foldStream' maps the
-- type of 'foldM', @(Stream m)@ is not foldable since it can only
-- return (non trivial) values within the monad @m@.
foldMStream :: Monad m => (b -> a -> m b) ->
  b -> Stream m a -> m b
foldMStream f b (Stream m) = m>>= (maybe (return b)
  (\(a , s) -> (f b a) >>= \b -> foldMStream f b s))
    

-- | same as above with non monadic fold function
foldStream :: Monad m => (b -> a -> b) ->
  b -> Stream m a -> m b
foldStream f = foldMStream (\b -> \a -> return (f b a))

    
{-
            
-- ** Monad streams vs monad lists

-- | The monad list item 
data ListI m a  =  Nil  | Cons a  (m (ListI m a))
                
-- | A type isomorphic to Stream m a (equivalent with ListT defined in List-0.6.2: List monad transformer and class)
type ListT m a = m (ListI m a)

-- | A type isomorphic to ListI m a
type StreamI m a = Maybe (a , Stream m a)

-}

            
-- ** Monad streams vs monad actions

-- | since any monad stream starts with an action, this allows the
-- following (safe) conversion mapping.
toStream :: Monad m => m (Stream m a) -> Stream m a
toStream  m = Stream $ m >>= next


-- | Need MonadRef m for Stream m to be a monad
instance MonadTrans Stream where
    lift m = Stream (m >>= \a -> return(Just (a,emptyStream))) 

-- | Running monad streams
runStream :: Monad m => Stream m () -> m ()
runStream (Stream m) = do 
   c <- m
   case c of
     Nothing -> return ()
     Just((),sc) -> runStream sc
    
-- * Horizontal monoid structure

-- | binds a monad action to a monad stream function. Sort of a heterogeneous bind.
bindToStream :: Monad m =>
  m a -> (a -> Stream m a) -> Stream m a
bindToStream m f = Stream $
  m >>= \a -> return $ Just (a, f a) 

-- | appends a monad action in front of a monad stream.
appendToStream  :: Monad m => m a -> Stream m a -> Stream m a
appendToStream  m s = bindToStream m (const s)

-- | appends a delay in front of a monad stream.
delayToStream  :: Monad m => m () -> Stream m a -> Stream m a
delayToStream  m (Stream ms) = Stream $ m>>ms
                               
-- | adds a value in front of a monad stream.
consToStream  :: Monad m => a -> Stream m a -> Stream m a
consToStream  a s = bindToStream (return a) (const s)


-- | empty stream
emptyStream :: Monad m => Stream m a
emptyStream = Stream (return Nothing)
                    
-- | concatenates two monad streams
--
-- In the context of reactive data flow programing with stream, the relevance of concatenating two monad stream one after the other is highly questionable.
concatStream :: Monad m => Stream m a -> Stream m a -> Stream m a
concatStream (Stream m) s  = Stream $ do
    c <- m
    case c of
      Nothing -> next s
      Just (a,sc) -> return $ Just (a, concatStream  sc s)

{-                     
-- | Horizontal stream semigroup
instance Monad m => Semigroup (Stream m a) where
  (<>) = concatStream
         
-- | Horizontal stream monoid
instance Monad m => Monoid (Stream m a) where
    mempty = emptyStream
-}


-- * Streams by iterations

-- ** Unconditional iteration


-- | returns the infinite monad streams obtained by iteration of a monad action function.
iterateStream :: Monad m =>
 (a -> m a) -> a -> Stream m a
iterateStream f a  = bindToStream (f a) (iterateStream f)

-- | iterates the given function until it produces 'Nothing'.
iterateStreamMaybe :: Monad m =>
  (a -> m (Maybe a)) -> a -> Stream m a
iterateStreamMaybe f a  = Stream $ do
  c <- f a
  return $ fmap (\b -> (a , iterateStreamMaybe f b)) c

        
-- ** Fixed number of iteration
        
-- | creates the monad stream that
-- repeatedly returns the arguement values for some fixed number of times.         
repeatN :: Monad m => Int -> a -> Stream m a
repeatN n a = Stream . return $
  if (n <= 0)  then Nothing
  else Just(a,repeatN (n-1) a)

-- | takes the nth first elements of a monad stream.       
takeN :: Monad m => Int -> Stream m a -> Stream m a
takeN n s = zipStreamWith const s
            (repeatN n ())


-- ** Iteration until
 
-- | returns the timed list of values obtained by iteration until
-- some monad action terminates. This is made simple by using monad
-- reference.
iterateStreamUntil :: MonadRef m =>
  MRef m b -> (a -> m a) -> a -> Stream m a
iterateStreamUntil r f a = Stream $ do
  tc <- tryReadRef r
  case tc of
    Just _ -> return Nothing
    Nothing -> next $ bindToStream (f a)
        (iterateStreamUntil r f)
             
             
-- * Filtering / selecting            
            
-- ** Take until

-- | runs an action and a monad stream, and returns the stream cut when the action is terminated.
takeStreamUntil :: MonadRef m =>
    m a -> Stream m b -> Stream m b
takeStreamUntil m s = Stream $ do
  r <- forkToRef m
  takeStreamUntilRef r s
    where
  takeStreamUntilRef r (Stream ms) = do
    rs <- forkToRef ms
    c <- parReadRef r rs
    case c of
      Left _ -> return Nothing
      Right Nothing -> return Nothing
      Right (Just (b,sc)) -> return $
        Just(b, Stream $ takeStreamUntilRef r sc)
              
              
-- ** filtering
                    
-- | filters a monad streams by some maybe function
filterMaybe :: MonadRef m =>
  (a -> Maybe b) -> Stream m a -> Stream m b
filterMaybe f (Stream m) = Stream $ do
  c <- m
  case c of
    Nothing -> return Nothing
    Just(a,mc) -> case (f a) of
      Nothing ->
        let Stream mr = filterMaybe f mc in mr
      Just b -> return $ Just (b, filterMaybe f mc)

-- | filters a monad streams by some maybe function
filterEither :: MonadRef m =>
  a -> b -> (a -> b -> c) -> Stream m (Either a b) -> Stream m c
filterEither a0 b0 f (Stream m) = Stream $ do
  c <- m
  case c of
    Nothing -> return Nothing
    Just(Left a, sc) -> return $ Just(f a b0, filterEither a b0 f sc)
    Just(Right b, sc) -> return $ Just(f a0 b, filterEither a0 b f sc)
                
-- | filters a monad streams by some boolean function
filterBool :: MonadRef m => (a -> Bool) -> Stream m a -> Stream m a
filterBool f (Stream m) = Stream $ do
  c <- m
  case c of
    Nothing -> return Nothing
    Just(a,mc) -> case (f a) of
      False -> let Stream mr = filterBool f mc in mr
      True -> return $ Just (a, filterBool f mc)
       

               

             
-- * Synchronous stream functions

-- | the type of synchronous function from /Stream m a/ to
-- /Stream m b/.
-- 
-- A function with several monad stream as inputs and one monad stream
-- as output is called synchronous when all the inputs streams
-- traversed in a synchronous way and the ouput stream is also
-- produced at the same rate.
--
-- Todo : an encapsulated type for synchronous stream function ?
type Sync m a b = Stream m a -> Stream m b


-- ** Zipping
 
-- | zips two monad streams at the speed of the slowest.
-- The archetypal example of a synchronous
-- function.
--  
-- In the category of synchronous stream functions, we have:
--
-- > Stream m (a,b) is the product of Stream m   and Stream m b
-- with /zipStream/ as pairing function  and /fmap (\(a,_) -> a)/ and /fmap (\(,b) -> b)/
-- as left and right projections.
zipStream :: Monad m =>
        Stream m a -> Stream m b -> Stream m (a,b)
zipStream =  zipStreamWith (,)

             
-- | a parameterized version of zip streaming.
zipStreamWith ::(Monad m) => (a -> b -> c) ->
        Stream m a -> Stream m b -> Stream m c
zipStreamWith f (Stream m1) (Stream m2)
 = Stream $ do
  c1 <- m1
  c2 <- m2
  case (c1, c2) of
    (Just (a1,sc1), Just(a2,sc2)) -> do
        let v = f a1 a2 
        return $ seq v $ Just(v, zipStreamWith f sc1 sc2)
    _ -> return Nothing


-- ** Synchronous const

-- | a synchronous version of const. This function is
-- usefull to force the reading of the input stream, and synchronize
-- the output at the frequency of the input.
constSync  :: Monad m => Stream m b -> Stream m a -> Stream m b                        
constSync = zipStreamWith const
         
-- ** Synchronous mealy machines

-- | creates a Mealy machine from a transition map with implicit hidden state (in the monad)
mapToStream :: Monad m => (a -> m b) ->
  Stream m a -> Stream m b                  
mapToStream f (Stream m) = Stream $ do
  c <- m
  case c of
    Nothing -> return Nothing
    Just (a,sc) -> do
      b <- f a
      return $ Just (b, mapToStream f sc)

-- | creates a Mealy machine with explicit state.
--
-- For efficiency, the state shall be of a
-- bounded size, the state update function making no allocation. See
-- 'Audio.varyingRetro' for an application to audio programing.
loopStream :: Monad m => s ->
  ((a , s) -> m (b ,  s)) ->  Stream m a -> Stream m b
loopStream s f (Stream m) = Stream $ do
  c <- m
  case c of
    Nothing -> return Nothing
    Just (a , sc) -> do
      (b , s') <- f (a , s)
      return $ Just (b , loopStream s' f sc)             
             
-- * Some tests

-- | outputs stream argument to sdtout                 
stream2Stdout :: Stream IO (Char,Char) -> IO ()
stream2Stdout (Stream m) = do
  c <- m
  case c of
    Nothing -> return ()
    Just((a1,a2),s) -> do
      putChar '('
      putChar a1
      putChar ','
      putChar a2
      putChar ')'
      stream2Stdout s

