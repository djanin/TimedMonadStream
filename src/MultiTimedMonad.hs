{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{-| 
Module      : MultiTimedMonad
Description : When timed monad handles two timescales
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

Timed monad with inner and outer durations / timescales.

-}


module MultiTimedMonad where

import Time
import TimedMonad
import MonadStream




-- * Time scale  (first order) changes

-- | Time scale changes from a inner duration type @i@ to an outer duration type @o@ with derivative type @d@
-- shall satisfy the following properties:
--
--
-- prop> backStep d (step d i) ==  i
--
-- prop> step d (backStep d o) == o
--
-- for every inner duration i, outer duration o and non zero speed d.
class (Num i, Num o, Num d) => ScaleChange i o d | d -> i, d -> o where
    initialSpeed :: d
    -- | initial derivative
    step :: d -> i  -> o
    -- | essentially encoding step d i = d*i
    backStep :: d -> o -> i
    -- | essentially encoding backStep d o = o/d
                
              
instance ScaleChange Micro Beat BPM where
    initialSpeed = BPM 60
    step (BPM t) (Micro d) = Beat $ t*(fromIntegral d)/60000000
    backStep (BPM t) (Beat d) = Micro $ fromIntegral (floor(d*60000000/t))
                            
        
-- * Timed monad with double timescales

-- | Timed state
data TS i o d
    = TS { innerTime :: Time i,
           outerTime :: Time o,
           timeSpeed :: d }

-- | returns the initial timed state induced some initial inner timestamp      
initState :: (ScaleChange i o d) => Time i -> TS i o d
initState ti = TS ti (Time 0) initialSpeed
               
-- | shift a state by some outer duration
shiftState :: ScaleChange i o d => o -> TS i o d -> TS i o d
shiftState o (TS ti to s)
    = TS (shift ti (backStep s o)) (shift to o) s
      -- we update both timestamps

      
-- | Double timed Action
data DTA m i o d a = DTA (TS i o d -> m (TS i o d, a))
                       
instance (Monad m) => Functor (DTA m i o d) where
    fmap f (DTA m) = DTA $ \s -> do
      (s1,a) <- m s
      return (s1, f a)
instance (Monad m) => Applicative (DTA m i o d) where
    pure a = DTA (\s -> return (s,a))
    (DTA m1) <*> (DTA m2) = DTA $ \s -> do
      (s1,f) <- m1 s 
      (s2,v) <- m2 s1
      return (s2, f v) 
instance (Monad m) => Monad (DTA m i o d) where
    return a = DTA (\s -> return (s,a))
    (>>=) (DTA m) f = DTA $ \s -> m s >>= \(s1,a) -> let (DTA m1) = f a in m1 s1

instance (Ord o, ScaleChange i o d, HasTimer m i) =>
    TimedMonad m o (DTA m i o d) where
        now = DTA (\s -> return (s, outerTime s))
        drift = DTA $ \s -> do
             tr <- getRealTime
             let dr = duration tr (innerTime s)
             return (s, step (timeSpeed s) dr)
        delay o | o <= 0 = DTA (\s -> return (s,()))
                | o > 0 =  DTA $ \s -> do
             let s1 = shiftState o s      
             waitUntil (innerTime s1)
             return (s1,())
        lift m = DTA (\s -> m >>= \a -> return (s,a))
        run (DTA m) = do
          ti <- getRealTime
          (_,a) <- m (initState ti)
          return a
                 
-- | sets the current speed to the given argument             
setSpeed :: Monad m => d -> DTA m i o d ()
setSpeed s = DTA $ \state  -> do
         let TS ti to _ = state
         return (TS ti to s,())

-- | goes faster by 20%
faster :: (Monad m, Fractional d) => DTA m i o d ()
faster = DTA $ \state  -> do
  let TS ti to s = state
  return (TS ti to (s * 1.2),())         

-- | goes slower by 20%
slower :: (Monad m, Fractional d) => DTA m i o d ()
slower = DTA $ \state  -> do
  let TS ti to s = state
  return (TS ti to (s * 0.8),())         


-- * Default musical IO monad
                                    
-- | Music IO action type with duration expressed in beats and tempo in beats per minutes (BPM)
type MusicIO = DTA IO Micro Beat BPM
    
-- | set the current speed to the given argument             
setTempo :: BPM -> MusicIO ()
setTempo s | s <= 0 = error $ "setTempo : forbidden negative tempo " ++ show s
setTempo s = setSpeed s

-- | Music IO streams             
type MusicIOS = Stream MusicIO
             
         
-- | Simple test for multi timed stream    
tryMusicIOS :: Int -> MusicIOS Int   
tryMusicIOS = iterateStream (\n -> delay 1 >> (lift . print) n >> drift >>= (lift . print) >> return (n+1))   
    
-- | Simple test for multi timed stream with change of tempo   
tryMusicIOSTempo :: Int -> MusicIOS Int     
tryMusicIOSTempo n = appendToStream (setTempo 1000 >> return (0)) (tryMusicIOS n)
