{-# LANGUAGE FlexibleInstances, BangPatterns #-}

{-|
Module      : Audio
Description : Audio specific stream function
Copyright   : (c) David Janin 2019
License     : see the LICENSE file in the distribution
Maintainer  : janin@labri.fr
Stability   : experimental

A (little) bunch of audio specific function and an interface with Jack audio connection toolkit via Henning Thielmann package. -}

module Audio where

import qualified Sound.JACK.Audio as JA
import qualified Sound.JACK.MIDI as JM

import qualified Sound.MIDI.Message as Midi
    
-- import qualified Sound.MIDI.Message.Channel as MidiChan
-- import qualified Sound.MIDI.Message.Channel.Voice as MidiMsg

    
import Sound.JACK.Audio (Sample)
-- import Sound.JACK.MIDI (RawEvent)
    
import IOStream
import MonadStream
import MonadStreamReference

import Foreign.Storable
-- import Foreign.Ptr (castPtr)    
    

import Buffer    

import Control.Concurrent (forkIO)

-- * Basic audio stream types    
    
-- | stereo audio stream    
type AudioSStream = Stream IO (Sample, Sample)
-- | stereo audio stream ref 
type AudioSStreamRef = StreamRef IO (Sample, Sample)


-- * Dynamically sized buffer
   
-- | In audio processing for music, state machine are often extended
-- with a (FIFO) buffering of states which lenght induced some
-- frequency.  That's what the following definition is used for. A
-- buffer state is a buffer of state values, a default (initial or
-- last outputed) state value, a (positive) size and a maximum size
data BufferState f s  = BufferState (Buffer s) !s  f f 

instance Show f => Show (BufferState f s) where
    show (BufferState _ _ s m)
        = "BufferState (Size : " ++ show s
           ++ ", MaxSize : " ++ show m ++")"
              
-- | gives the last value read by the buffer (but leave the buffer unchanged)   
getLast :: BufferState f s -> s    
getLast ( BufferState _ s  _ _) = s
    

-- | Returns an empty initial buffer
initBuffer :: (RealFloat f, Storable s) => s -> IO (BufferState f s)
initBuffer a = do
  ch <- newEmptyBuffer
  return $ BufferState ch  a  0 0

-- | updates the size of the buffer (to zero if negative)      
updateSizeBuff ::  RealFloat f => (f, BufferState f s) -> BufferState f s
updateSizeBuff (k , BufferState ch a  s _)
    = BufferState ch a  s (max 1 (k + 1))

-- | feedback buffer handling.
updateBuffer :: (RealFloat f , Storable s) =>
                s -> BufferState f s -> IO (s , BufferState f s)
-- first argument is new received value, second arguement is new state input
updateBuffer a (BufferState ch  a0  s m) = do
  writeBuffer ch a  -- add the new value to the buffer, now of size s+1
  (a1, s1)  <- case  (m <= (s + 1), m <= s) of
       -- buffer too small, default last value kept and size increase
      (False,_) -> return (a0,s+1)
      -- buffer essentially of the right size with
      -- m <= s + 1 < m + 1
      -- last value updated from buffer, size sligtly increase (for "real" size)
      (True,False) -> do
        let !s2 = s+s+1 - m
        a2 <- readBuffer ch
        return (a2,s2)
      -- feedback channel too big (at least two), new last value and
      -- buffer decrease
      (True,True) -> do
        _ <- readBuffer ch
            -- dropping one channel value
        a2 <- readBuffer ch
            -- taking the next one (achieving a correct sized buffer
            -- may take several stAuAueps)
        return (a2,s-1)
  return (a,BufferState ch a1 s1 m)


-- * Feeback with dynamically sized buffer


-- | Controled feedback. First stream parameter is buffer size, second
-- stream parameter is feedback coeficient.
varyingRetro :: RealFloat s => Storable b => b -> c -> (c -> a -> b -> b)
             -> Stream IO s -> Stream IO c -> Stream IO a -> Stream IO b   
varyingRetro b c0 f ss sc sa = toStream $ do
  s0 <- initBuffer b
     -- states are pairs (buffer,feeback coef) 
  let delta (a,(s,c)) = do
        (b,s1) <- updateBuffer (f c a (getLast s)) s
        return (b,(s1,c))          
      delta' (Left m) (a,(s,c)) = delta (a,(updateSizeBuff (m,s),c))
      delta' (Right c) (a,(s,_)) = delta (a,(s,c))
      sf = fmap delta' (mergeStream ss sc)       
  return $ loopStreamWithControl  (s0,c0)  delta sf sa

         

-- * various sample utility function

-- | Lifts a sample function to stereo samples
mapS :: (a1 -> a2 -> a3) -> (a1, a1) -> (a2, a2) -> (a3, a3)
mapS f (!x1,!x2) (!y1,!y2) = let r1 = f x1 y1
                                 r2 = f x2 y2
                              in (seq r1 r1, seq r2 r2)

-- | Merge two stereo samples by taking their average
avg :: (Fractional a) => (a,a) -> (a,a) -> (a,a)
avg = mapS (\x y -> (x + y)/2)

-- | number of samples per period at 44100 Hz sampling rate for a
-- given frequency
freqToSample :: RealFloat f => f -> f
freqToSample f = let ! n = 44100 / f in n

-- | frequency from MIDI pitch
pitchToFreq :: RealFloat f => f -> f
pitchToFreq p = 442 * 2**((p - 69)/12) -- A at 442 Hz

-- | frequency from MIDI pitch
freqToPitch :: RealFloat f => f -> f
freqToPitch f = 69 + 12 * (logBase 2 (f/442)) -- A at 442 Hz
                
-- | number of samples per period at 44100 Hs sampling for a given (MIDI) pitch
-- pitchToSample :: RealFloat f => Int -> f
pitchToSample :: RealFloat f => f -> f
pitchToSample p = let ! n = freqToSample (pitchToFreq p) in n
         
-- | returns a sinusoid with frequency f
osc :: RealFrac a => a -> AudioSStream
osc f = oscN 0 (floor (44100/f)) 
    where
  oscN :: Int -> Int -> AudioSStream       
  oscN n k = Stream $ do
      v <- return $ sin ( 2*pi * fromIntegral (mod n k) / fromIntegral n)
      return $ seq v  $ Just ((v,v), oscN (n+1) k)

-- | returns a sinusoid based of list of frequencies
oscF :: [Sample]  -> AudioSStream
oscF [] = mempty
oscF (f:l) = foldl (zipStreamWith avg) (osc f) (map osc l)

-- | a oscillating panning coeficient for stereo signal. The arguement
-- is the frequency of the oscillation.
pan :: (Monad m, Floating t) => t  -> Stream m (t, t)
pan f = oscN 0 f where
  oscN n f = Stream $ do return $ Just ((absSin f n, (1-absSin f n)), oscN (n+1) f)
  absSin f n = abs(sin ((n* 2*pi * f)/44100))
                        
-- | amplifies the signal by the given factor                
amp :: (Monad m, Floating a) => a -> Stream m (a,a) -> Stream m (a,a)       
amp f  = fmap (ampS f)
    where
      ampS f (x,y) = ((2 ** f) *x,(2 ** f)*y)

-- * Connecting with Jack


-- ** Jack audio
                     
-- | Runs, via Jack, a function from strereo audio stream to stereo audio stream. Such a function MUST be synchronous:
--
-- /each stero sample on the input is mapped to a stereo sample on the output/
--
-- The (first) integer arguement is an internal buffer size for haskell. It
-- works fine with just the same size as set in Jack.  With jhack
-- interface, such a buffer size determines the frequency at which
-- Jack shall call back Haskell.
playAudioJack  :: Int ->
 (AudioSStream -> AudioSStream) -> IO ()
playAudioJack  n f = do
  g <- streamToMapN n  (0,0) f
  _ <- forkIO $ JA.mainStereo g
  return ()

-- ** Jack midi

-- | Plays a MIDI stream with Jack. Here, we give the step MIDI
-- function to be used. See 'playAudioJack' code to make the same with
-- a (synchronous) function over streams of MIDI events.
playStepMIDIJack  ::
 (Midi.T -> IO Midi.T) -> IO ()
playStepMIDIJack  f = do
  let g (x,msgIn) = do
        msgOut <- f msgIn
        return  (x, msgOut) 
  _ <- forkIO $ JM.main (const g)
  return ()


