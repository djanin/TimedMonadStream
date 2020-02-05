{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies
  , TypeFamilies, FlexibleInstances  #-}

{-| 
Module      : MonadReference
Description : Monad references bound to running monad actions
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable


Playing with class around the idea of monad references bound to
running monad actions.  This module and the equational theory behind is detailed in :
Monadic references: a equational modeling of asynchronous concurrent programming

This module reuses, but also extends or restricts some idea of the
@async@ package. To some extent, one can also understand monad
references as certain kind of future values.  However, monad
references are both restricted kind of future values and, thanks to
such restrictions satisfies more properties than that faily general
concept.

There are many other packages that play with these
ideas, especially related with streaming. We have browsed many of them
and, so far, we haven't found any that closely follows the ideas
exposed here. Any unknown relationship to existing work (libraries or
articles) would be warmly welcome.

Simply said, a monad reference shall be a reference to a launched
monad action that allows for reading or replaying these actions
essentially with no (or harmless) side effects. Another way to say it: 

 /monad action programs can safely and freely be duplicated, combined,/
 /etc., in a pure fonctional programing style; in general, this is no/
 /longer true for runing monad actions that are uniquely located, by/
 /their side effects, in the spacetime of their underlying monad states;/
 /monad references, uniquely bound to running monad actions, offer a way/
 /to reinject some data, dynamically produced by running monad actions,/
 /into the safe, quiet and robust realm of pure fonctional programing./


We have put some efforts in designing the corresponding type classes
in order to understand what operations shall be primitive therefore
hard coded in instances, and what operations may derive from these
primitive ones.  However, the laws that shall be satisfied by
instances are not settled yet though we believe that there could be
some insteresting links with inverse semigroup theory and restriction
categories.

We provide three sets of instances based on:

* the Identity monad, for not so simple test purposes,

* the IO monad, closely related with @async@ lib,

Feel free to experiment, comment, possibly debug, and provide any feedbacks.


Remark : In FRP litterature, there is the notion of a reactive value,
that is, a value that can be set (once), and that can be attached (as
many times as one wishes) to a callback function. But this is a concept
known since the 70s as a future or promise value. 

The monad references proposed here are somehow strongly related (as well) with promises.
We do hope that our axiomatization proposal does constitute a step of progress, despite the
cleverness of the ancients :-)


  -}


module MonadReference where


import Control.Monad.Identity    
       
import Control.Concurrent.MVar  (MVar(), newEmptyMVar, putMVar,
                                     readMVar, tryReadMVar, tryPutMVar, takeMVar)
    
import Control.Concurrent (forkIO)
    
-- * Monads with references

{-| The class @MonadRefCore m@ defines the type @(MRef m a)@ which every
element is uniquely bound to a running monadic action
created by 'forkToRef'. A monad action can be re-executed in some sense via
its associated monad reference (see 'readRef'), although this is done
in a safe and robust way, essentially with no (or harmless) side efefct.

The following two series of laws shall be satisfied for any monad
actions @m@ and any monad action references @r@ :

Basic semantic laws :

> (1) m = forkToRef m >>= readRef
> (2) forkToRef . readRef = return
> (3) forkToRef( m >>= f) = (forkToRef m) >>= \r -> forkToRef( readRef r >>= f)

these rules allow for proving that:

        *  @(m . MRef m)@ is a functor with 'fmapRef',
        * 'forkToRef' is a natural transformation from @m@ to @(m . MRef m)@

Concurent semantic law (expressed by idempotence and commutativity)

> (4) readRef r = readRef r >> readRef r
> (5) readRef r1 >>= \x1 -> readRef r2 >>= \x2 -> return (x1,x2)
>        =  readRef r2 >>= \x2 -> readRef r1 >>= \x1 -> return (x1,x2)
> (6) forkToRef m1 >>= \r1 -> forkToRef m2 >>= \r2 -> return (r1,r2)
>        =  forkToRef m2 >>= \r2 -> forkToRef m1 >>= \r1 -> return (r1,r2)

where all equality above shall be understood as /observably
equivalent/, that is, having the same type, when run, returning
the same value and performing the same /observable/ side effects on
the underlying (series of) monad state(s).
-}
class Monad m => MonadRefCore m where
    type  MRef m :: * -> *
    -- | launches a monad action and returns (in no time) a reference to it.             
    forkToRef :: m a -> m (MRef m a)
    -- | reads the returned value of a referenced action when available.
    readRef :: MRef m a -> m a


{-| The class @MonadRef m@ extend the class @MonadRefCore m@ with 
various additional primitives. 
--
Warning : the function 'parRun' may behave non determnistically
for it involved a race between the two referenced monad actions.
-}
class MonadRefCore m => MonadRef m where
{-
    type  MRef m :: * -> *
    -- | launches a monad action and returns (in no time) a reference to it.             
    forkToRef :: m a -> m (MRef m a)
    -- | reads the returned value of a referenced action when available.
    readRef :: MRef m a -> m a
-}
    -- | tries to read the returned value of a referenced
    -- action. Returns in no time either nothing when the action is
    -- not terminated yet or just the returned value when the action
    -- is terminated.
    --
    -- Warning: there is the possibility of a  race as illustrated by the action
    --
    -- > forkToRef (return "foo") >>= tryReadRef
    --           
    -- that can return @Nothing@.
    tryReadRef :: MRef m a -> m (Maybe a)
    -- | returns the value of the earliest terminated referenced action after that action is terminated.
    --
    -- Warning: there is a race possibility as illustrated by the action
    --
    -- > do {r1 <- forkToRef (return "foo1"); r2 <- forkToRef (return "foo2"); parReadRef r1 r2}
    --           
    -- that can return either @Left "foo1"@ or @Right "foo2"@.                  
    parReadRef :: MRef m a -> MRef m b -> m (Either a b)

                  
-- * Monad references vs promises

-- | applies a function under the reference. There are essentially
-- three well-typed option for /fmap/ given by:
--
-- > (1) fmapRef f m = fork (m >>= readRef >>= return . f)
-- > (2) fmapRef f m = m >>= \r -> forkToRef (readRef r >>= return . f)
-- > (3) fmapRef f m = m >>= readRef >>= \a -> forkToRef (return (f a))
--
-- that differs by their execution time (in a strict monad).  In the
-- case /m/ is a promise, that is, an action equivalent to /forkToRef
-- . m'/ then (1) and (2) are equivalent.
--
-- Observe however, that only option (2) preserves the duration of
-- /m/. This is why it is implented as (2).
--
-- One can also show that, the coherence property:
--
-- > fmapRef f m = bindRef m (returnRef . f )
--
-- is satisfied with 'bindRef' and 'returnRef' defined below, option
-- (2) choosen both for 'fmapRef' and 'bindRef'.
fmapRef :: MonadRef m => (a -> b) -> m (MRef m a) -> m (MRef m b)
fmapRef f m = m >>= \r -> forkToRef (readRef r >>= return . f)

-- | returns a monad reference to a value
returnRef :: MonadRef m => a -> m (MRef m a)
returnRef = forkToRef . return
-- | binds a "promise" to a "callback" function (following OCaml
-- /async/ terminology). There are essentially four well-typed options for
-- such a binding:
--
-- > (1) bindRef m f = forkToRef (m >>= readRef >>= f >>= readRef)
-- > (2) bindRef m f = m >>= \r -> forkToRef (readRef r >>= f >>= readRef)
-- > (3) bindRef m f = m >>= readRef >>= f >>= \r -> forkToRef (readRef r)
-- > (4) bindRef m f = m >>= readRef >>= f >>= readRef >>= \b -> forkToRef (return b)
--
-- that differs only by their execution time (in a strict monad)
-- assuming /forkToRef/ returns instantaneously. Option (1)
-- returns instantaneously. Option (2) returns when /m/
-- return. Option (3) returns later. Option (4) returns the
-- latest. When /m/ is a promise, that is, an action equivalent to
-- /forkToRef . m'/ then (1) and (2) are equivalent.
bindRef :: MonadRef m => m (MRef m a) -> (a -> m (MRef m b)) -> m (MRef m b)
bindRef m f = m >>= \r -> forkToRef (readRef r >>= f >>= readRef)

-- | runs two actions and returns the value the earliest terminated action when that action is terminated.
parRun :: MonadRef m => m a -> m b -> m (Either a b)
parRun m1 m2 = do
  r1 <- forkToRef m1
  r2 <- forkToRef m2
  parReadRef r1 r2

instance MonadRefCore Identity where
    type MRef Identity = Identity
    forkToRef = Identity
    readRef = id
    
instance MonadRef Identity where
    tryReadRef (Identity a) = Identity (Just a)
    parReadRef (Identity r) _ = Identity (Left r)

{-
-- encoded with Async                             
instance MonadRefCore IO where
    type MRef IO = A.Async
    forkToRef m = A.async m
    readRef = A.wait
                
instance MonadRef IO where
    tryReadRef = undefined -- impossible with Async ?
    parReadRef = A.waitEither

-}

-- encoded with MVar

-- | The hidden type of IO references
newtype RefIO a = RefIO (MVar a)
     
instance MonadRefCore IO where
    type MRef IO = RefIO 
    forkToRef m = do
      v <- newEmptyMVar 
      _ <- forkIO (m >>= putMVar v)
      return (RefIO v)
    readRef (RefIO v) = readMVar v

              
instance MonadRef IO where
    tryReadRef (RefIO v) = tryReadMVar v
    parReadRef r1 r2 = do
      v <-  newEmptyMVar 
      _ <-  forkIO (readRef r1 >>= (tryPutMVar v . Left) >> return ()) 
      _ <-  forkIO (readRef r2 >>= (tryPutMVar v . Right) >> return ()) 
      takeMVar v

{-            
-- * Pathological instances

-- | Non side effect free reads
instance MonadRefCore IO where
  type MRef IO = IO
  forkToRef = return
  readRef = id

-- | Non instantaneous forks
instance MonadRefCore IO where
  type MRef IO = MVar 
  forkToRef m = m >>= newMVar
  readRef = readMVar

-- | Execution of forked deffered to first reference reading
data DeferredIO a = DeferredIO (MVar (Either (IO a) a))
    
instance MonadRefCore IO where
  type MRef IO =  DeferredIO
  forkToRef m = newMVar (Left m) >>= return . DeferredIO
  readRef (DeferredIO v) = do 
     c <- takeMVar v
     a <- case c of 
        Left m -> m
        Right a -> return a
     putMVar v (Right a)
     return a
               
-}
