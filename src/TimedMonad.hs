{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies
  , TypeFamilies, FlexibleInstances  #-}

{-| 
Module      : TimedMonad
Description : When monad action takes time
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

Playing with class around the idea of a timed monad, that is, a monad
which action takes explicit time when ran.

A timed monad is essentially the extension of a monad which underlying
state embeds:

* a notion of /expected\/specified/ timestamp increased after the
execution of any timed monad action,

* a notion of /actual\/real/ timestamp provided by the underlyign
monad,

* the derived notion of /time drift/, that is, the difference between
actual and expected timestamp,

together with:

* a 'delay' function that creates an action that increase the
specified timestamps by the given duration arguement and but that
waits that this new specified timestamp to be passed /for real/,
therefore minimizing the timedrift,


* a 'lift' function that turns an action from the underlying monad
into a timed action assumed to be /instantaneous/; the time drift
necessarily increases when executing such a lifted action; it shall
only be used to lift reasonnably instantaneous actions, e.g.
'putChar' in the IO Monad, but certainly not blocking action,
e.g 'getChar' in the IO monad,
 

* a (derived) 'timedLift' function that turns an action from the
underlying monad into a timed action taking into account its /real
duration/; it can be used with action that does take time such as
blocking action, e.g 'getChar' in the IO monad,


The expected duration of an timed monad action is the difference
(i.e. the duration distance) between its output state expected time
stamp and its input expect timestamp.

A /temporaly correct/ timed monad action is a monad where the 'drift'
remains bounded. Such a bound gives the /time accuracy/ of that timed
monad action. It is /programmer duty/ to insert adequate delays (and
other things) to insure such a correctness.

-}

module TimedMonad where

import Time

import MonadStream    

    
-- * Timed monads
                     
-- | timed extension of a monad m by a monad t with duration space d    
class (Ord d, Num d, Monad m, Monad t) => TimedMonad m d t | t -> m , t -> d where
    -- | instantaneously returns the specified current timestamps as
    -- evaluated accumulating the specified durations (could be
    -- private, used internally or for debug).
    now :: t  (Time d)
    -- | instantaneously returns the current timedrift (could be
    -- private, used internally of for performance measure).
    drift :: t d
    -- | waits for the specified duration.
    delay :: d -> t ()
    -- | lifts an action from the underlying monad treating it as
    -- instantanenous. Can be used with /reasonably/ instantaneous
    -- action, e.g 'IO.printChar' in the IO monad.  Shall not be used
    -- with blocking action, e.g. 'Prelude.getChar' in the IO monad.
    lift :: m a -> t a
    -- | turns a timed action back into an untimed one, the specified
    -- initial time stamps sets to the current real timestamp,
    -- therefore with a zero intial timedrift.
    run :: t a -> m a
           
-- | returns the real current timestamp as evaluated by the
-- underlying monad (could be private, used internally or for
-- debug).
realNow :: TimedMonad m d t => t (Time d)
realNow = do {t <- now; d <- drift; return (shift t d)}
-- | lifts a monad action taking into account its real
-- duration. Especially usefull for blocking action such as
-- 'getChar'.
timedLift ::TimedMonad m d t => m a -> t a
timedLift m = do {a <- lift m; d <- drift; delay d; return a}

-- | Returns the specified duration of an action, dropping its
-- returned value but performing its side-effects.
dur :: TimedMonad m d t => t a -> t d
dur m = do {t0 <- now; _ <- m; t1 <- now; return (duration t1  t0)}
              
-- * Timed monad instances from timers
              
-- | The simplest timed lifting of a monad : just add timing
-- information to the (unknown underlying) monad state.
data TA m d a = TA (Time d -> m (Time d, a))

instance (Monad m) => Functor (TA m d) where
    fmap f (TA m) = TA $ \s -> do
      (s1,a) <- m s
      return (s1, f a)
             
instance (Monad m) => Applicative (TA m d) where
    pure = return
    mf <*> m = mf >>= \f -> m >>= \a -> return (f a)

instance (Monad m) => Monad (TA m d) where
    return a = TA (\s -> return (s,a))
    (>>=) (TA m) f = TA $ \s -> m s >>= \(s1,a) -> let (TA m1) = f a in m1 s1

                    
instance (Monad m, HasTimer m d)
   => TimedMonad m d (TA m d) where
  now = TA (\s -> return (s,s))
  drift = TA $ \s -> getDrift s >>= \d -> return (s, d)
  delay d | d <= 0 = return ()
  delay d | d > 0 = TA $ \s -> do
     dr <- getDrift s
     waitUntil (shift s (d - dr))
     return (shift s d,())
  lift m =  TA $ \s -> m >>= \a -> return (s,a)
  run (TA m) = getRealTime >>= m >>= \(_,a) -> return a


-- | timed IO actions            
type TIO = TA IO Micro

-- | timed IO streams    
type STIO = Stream TIO


-- | Simple test for timed stream    
trySTIO :: Int -> STIO Int   
trySTIO = iterateStream (\n -> delay 1000000 >> (lift . print) n >> return (n+1))   

    
