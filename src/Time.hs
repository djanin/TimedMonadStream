{-# LANGUAGE MultiParamTypeClasses,  BangPatterns #-}

{-| 
Module      : Time
Description : Timstamps, durations and utility functions
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

Timescales from duration space. Various duration space and some 
utility functions for time handling.

-}


module Time where


import qualified System.Clock as C --  (getTime, sec, nsec, Clock(Monotonic)) as C

import System.Timeout (timeout)

import Control.Concurrent (threadDelay, MVar(..),readMVar)    
    
-- * Timescales and durations
    
-- | time scale associated to a duration space d
newtype Time d = Time d deriving (Eq,Ord)

-- | duration t1 t2 gives the duration from t2 to t1. It is positive
-- when |t2| occurs after |t1|.
duration :: Num d => Time d -> Time d -> d
duration (Time d1) (Time d2) = (d1 - d2)

-- | shift t d increases time t by duration d
shift :: Num d => Time d -> d -> Time d
shift (Time d1) d2 = Time (d1 + d2)

-- ** Duration in micro seconds

-- | Default microsecond duration type        
newtype Micro = Micro Int deriving (Show, Eq, Ord)

instance Num (Micro) where
    (+) (Micro a) (Micro b) = Micro (a + b)
    (*) (Micro a) (Micro b) = Micro (a*b)
    abs (Micro a) = Micro (abs a)
    signum (Micro a) = Micro (signum a)
    fromInteger n = Micro (fromInteger n)
    negate (Micro a) = Micro (negate a)
                     
instance Enum (Micro) where
    toEnum a = Micro (toEnum a)
    fromEnum (Micro a) = fromEnum a           
               
instance Real (Micro) where
    toRational (Micro a) = toRational a

instance Integral (Micro) where
    toInteger (Micro a) = toInteger a
    quotRem (Micro a) (Micro b) = let (c,d) = quotRem a b
                                in (Micro c, Micro d)

-- ** Musical time in Beat

-- | Symbolic duration in numbers of beats
newtype Beat = Beat Double deriving (Eq, Ord, Show)

instance Num (Beat) where
    (+) (Beat a) (Beat b) = Beat (a + b)
    (*) (Beat a) (Beat b) = Beat (a*b)
    abs (Beat a) = Beat (abs a)
    signum (Beat a) = Beat (signum a)
    fromInteger n = Beat (fromInteger n)
    negate (Beat a) = Beat (negate a)

                      
-- | Musical tempo in beat per minutes (BPM)
newtype BPM  = BPM Double deriving (Eq, Ord, Show)

instance Num (BPM) where
    (+) (BPM a) (BPM b) = BPM (a + b)
    (*) (BPM a) (BPM b) = BPM (a*b)
    abs (BPM a) = BPM (abs a)
    signum (BPM a) = BPM (signum a)
    fromInteger n = BPM (fromInteger n)
    negate (BPM a) = BPM (negate a)

instance Fractional BPM where
    (/) (BPM a) (BPM b) = BPM (a / b)
    fromRational = BPM . fromRational



-- * Timer               

-- | When a monad is equipped with a timer
class (Ord d, Num d, Monad m) => HasTimer m d where
    -- | returns the real time stamp measured.
    getRealTime :: m (Time d)
    -- | waits until the specified time stamps is passed for real.
    waitUntil :: Time d -> m ()
    -- | computes the difference between the real current timestamp
    -- and the one passed in parameter.
    getDrift :: (Time d) -> m d
    getDrift t = do { r <- getRealTime; return (duration r t) }


instance HasTimer IO Micro where
    getRealTime = getMTime
    waitUntil = waitM
                    
                    
-- * In the IO Monad

-- | returns system timestamp measured in microseconds 
getMTime :: IO (Time Micro)
getMTime = do
  t <- C.getTime C.Monotonic
  let !r = fromIntegral $ 10^6*(C.sec t) + div (C.nsec t) 1000
  return (Time (Micro r))         
    


-- | waits until timestamp in miscroseconds is passed
waitM :: Time Micro -> IO ()
waitM (Time (Micro t)) = do
  (Time (Micro r)) <- getMTime
  threadDelay (t - r)

-- | waits until timestamp in miscroseconds is passed unless MVar is filled
waitMUnless :: Time Micro -> MVar a -> (a -> IO ()) -> IO ()              
waitMUnless (Time (Micro t)) mv f = do
  (Time (Micro r)) <- getMTime
  c <- timeout (t - r) (readMVar mv)
  case c of
    Nothing -> return ()
    Just a -> f a
         

{-              
-- * Time keeper


data TimeKeeper d = TimeKeeper 

-}
