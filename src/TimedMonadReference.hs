{-# LANGUAGE Safe, FlexibleInstances,  TypeFamilies #-}

{-| 
Module      : TimedMonadReference
Description : Timed Monad references bound to running monad actions
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

How to extend timed monad with references.

-}


module TimedMonadReference where

import Time
import TimedMonad
import MonadReference

    
-- * Timed monad reference
                   
-- | A timed monad reference is the uniquely defined descriptor of a
-- running timed monad action obtained by forking a timed monad  action with
-- 'forkToRef'. 
data TRef m d a = TRef (Time d) (MRef m (Time d, a))

instance (MonadRef m, HasTimer m d) => MonadRefCore (TA m d) where
    type instance MRef (TA m d) = TRef m d
    forkToRef (TA m)
      = TA $ \s -> do {r <- forkToRef (m s); return (s, TRef s r) }
    readRef (TRef _ r)
      = TA $ \s -> do {(t,a) <- readRef r; return (max s t, a) }

instance (MonadRef m, HasTimer m d) => MonadRef (TA m d) where
    tryReadRef (TRef _ r)
      = TA $ \s -> do {c <- tryReadRef r; case c of
         Nothing -> return (s, Nothing)
         Just (t,a) -> return (max s t, Just a) }
    parReadRef  (TRef _ r1)  (TRef _ r2)
      = TA $ \s -> do {c <- parReadRef r1 r2; case c of
         Left (t,a) -> return (max s t, Left a)
         Right (t,b) -> return (max s t, Right b) }

                        
-- | Returns the (specified) duration of a referenced timed action, possibly waiting for its termination
durTRef :: (Ord d, Num d, MonadRef m, HasTimer m d)
           => TRef m d a -> TA m d d
durTRef (TRef t0 r)
    =  TA $ \s -> do {(t,_) <- readRef r; return (max s t, duration t t0)}

-- | Last the same (specified) duration as teh referenced action.
delayTRef :: (Ord d, Num d, MonadRef m, HasTimer m d) => TRef m d a -> TA m d ()
delayTRef r =   do {t1 <- now; d <- durTRef r; t2 <- now; delay (d - duration t2 t1)}

          
-- | replays a referenced timed actions that is, last the same
-- specified duration and returns the same value. A difference with
-- the referenced action, |replayTRef| essentially has no side-effects
-- but waiting.
replayTRef :: (Ord d, Num d, MonadRef m, HasTimer m d)
              => TRef m d a -> TA m d a
replayTRef r = do {  t1 <- now; d <- durTRef r; a <- readRef r; t2 <- now;
                     delay (d - duration t2 t1); return a}     
        
-- | replays a referenced action but applying an expansion (or shrink)
-- function to its duration. Warning : in case of a shrink, it can
-- violate temporal causality and therefore fails to apply the
-- expected shrink. We have:
--
-- > replayTRef r = expandTRef id r
--
-- for every timed reference @r@.
expandTRef :: (Ord d, Num d, MonadRef m, HasTimer m d)
              => (d -> d) -> TRef m d a -> TA m d a         
expandTRef f r = do {t1 <- now; d <- durTRef r; a <- readRef r; t2 <- now;
                     let d1 = f d - duration t2 t1 in delay d1; return a}      
  
