{-# LANGUAGE BangPatterns #-}

{-|
Module      : DB1
Description : Connecting one (very specific) midi device and building FARM experiment
Copyright   : (c) David Janin 2019
License     : see the LICENSE file in the distribution
Maintainer  : janin@labri.fr
Stability   : experimental

This can be red as a usage exemple, eventually defining function 'presentationFARM19' that was run throughout our presentation at FARM 2019. The DB1 is a nine slider midi interface, generally used (though not here) to control organ registers.

 -}


module DB1 where

    

import Control.Concurrent.Chan


import qualified Sound.MIDI.Message as Midi
    
import qualified Sound.MIDI.Message.Channel as MidiChan
import qualified Sound.MIDI.Message.Channel.Voice as MidiMsg
    
import Audio

import Sound.JACK.Audio (Sample)
    
import MonadStream (Stream, filterMaybe, filterEither)

import MonadStreamReference(StreamRef, spanStream
                   , forkStreamToRef, readStreamRef)
    
import IOStream (makeStreamFromChan)
    
-- * Connecting an adhoc midi input device (DB1)

-- | Turns a midi input value (from our adhoc DB1 with 9 sliders) to a
-- pair of integer (slider numebr, slider value)
getDB1Control :: Midi.T -> (Int,Sample)
getDB1Control (Midi.Channel (MidiChan.Cons _ (MidiChan.Voice (MidiMsg.Control  a  b))))
   = let !r = MidiMsg.fromController a - 12
         !rb = fromIntegral $ b     
     in ( r , rb)
getDB1Control _ = error "getDB1Control : invalid DB1 value"
     

-- | adhoc show instance of midi messages from DB1. A good exercise to
-- catch Henning Thielmann's style module structure with one structure
-- T in each and every module (was it typical in ML programing world
-- ?)
instance Show (Midi.T) where
    show (Midi.Channel (MidiChan.Cons _ (MidiChan.Voice (MidiMsg.Control a b))))
        = "Control : " ++ show (MidiMsg.fromController a - 12)
          ++ " Value : " ++ show b
    show _ = "unused with DB1"

-- | if ever you wonder what are the received midi signal, insert such
-- a monadic copycat function
copyCatPrint :: Show a => a -> IO a
copyCatPrint a = print a >> return a


-- | stream the input midi events into a channel ('Chan.Chan') that
-- can later be converted into a mponad stream via
-- 'IOStream.makeStreamFromChan'
streamDB1ToChan :: IO (Chan (Int,Sample))
streamDB1ToChan = do
  ch <- newChan
  let f a = do
        let b@(n,v) = getDB1Control a
        writeChan ch b
        print b
        print (pitchToFreq v)      
        print (pitchToSample v)      
        return a
  playStepMIDIJack f
  return ch             


-- | an adhoc filtering of DB1 stream ref of control values       
filterDB1Ref1 :: Int -> (Sample -> a)
     -> StreamRef IO (Int, Sample) ->  Stream IO a                
filterDB1Ref1 id  f r
  = let filter (c,v) = case (c == id) of
          True -> Just (f v)
          False -> Nothing
    in  filterMaybe filter (readStreamRef r)              

-- | another adhoc filtering of DB1 stream ref of control values        
filterDB1Ref2 :: Int -> Int -> (Sample -> a) -> (Sample -> b)
     -> StreamRef IO (Int, Sample) ->  Stream IO (Either a b)                
filterDB1Ref2 id1 id2 f1 f2 r
  = let filter (c,v) = case (c == id1 , c == id2) of
          (True,False) -> Just (Left (f1 v))
          (False,True) -> Just (Right (f2 v))
          (False,False) -> Nothing
          (True,True) -> error "filterDB1Ref2 : this shoudl not append"
    in  filterMaybe filter (readStreamRef r)              

        
        
-- | a stereo feeback loop exemples with control from DB1. With
-- /feedbackDB1Ref r s si co s/ we have the (asynchronous) control
-- stream /r/, some initial coefs /s/, /si/ and /co/ and the
-- (synchronous) audio stream /s/.
feedbackDB1Ref ::  StreamRef IO (Int,Sample) -> Sample -> Int -> Int 
           -> Stream IO (Sample,Sample) -> Stream IO (Sample,Sample)
feedbackDB1Ref r s sizeChan coefChan =
  let amp :: Sample -> Sample -> Sample -> Sample
      amp c x y = let !v = x + c*y in v
      size :: Sample -> Sample                     
      size n =  let !v =  n*s in v
      coef :: Sample -> Sample                   
      coef n =  let !v = n / 127 in v  
  in varyingRetro (0,0) 0 (\c -> mapS (amp c))
            (filterDB1Ref1 sizeChan size r)
            (filterDB1Ref1 coefChan coef r)


-- | another stereo feeback loop exemples with control from DB1,
-- essentially with same kind of parameters.
feedbackDB1Ref2 ::  StreamRef IO (Int,Sample) -> Sample -> Int -> Int -> Int
           -> Stream IO (Sample,Sample) -> Stream IO (Sample,Sample)
feedbackDB1Ref2 r s sizeChan coefInChan coefOutChan =
  let amp :: Sample -> Sample -> Sample -> Sample -> Sample
      amp c1 c2 x y = let !v = c1*x + c2*y in v
      size n =  let !v = n*s in v
      coef n =  let !v = n / 127 in v  
  in varyingRetro (0,0) (0,0) (\(c1,c2) -> mapS (amp c1 c2))
      (filterDB1Ref1 sizeChan size r)
      (filterEither 0 0 (\a -> \b -> (a,b)) $
               filterDB1Ref2 coefInChan coefOutChan
                                 coef coef r)

-- | another stereo feeback loop exemples with control from DB1,
-- essentially with same kind of parameters.
feedbackDB1Ref3 ::  StreamRef IO (Int,Sample) -> Int -> Int -> Int
           -> Stream IO (Sample,Sample) -> Stream IO (Sample,Sample)
feedbackDB1Ref3 r sizeChan coefInChan coefOutChan =
  let amp :: Sample -> Sample -> Sample -> Sample -> Sample
      amp c1 c2 x y = let !v = (c1*x + c2*y)/(1+c1+c2) in v
      size n =  let !v = (pitchToSample n) in v
      coef n =  let !v = n / 127 in v  
  in varyingRetro (0,0) (0,0) (\(c1,c2) -> mapS (amp c1 c2))
      (filterDB1Ref1 sizeChan size r)
      (filterEither 0 0 (\a -> \b -> (a,b)) $
               filterDB1Ref2 coefInChan coefOutChan
                                 coef coef r)

 -- was      

-- | Program ran during FARM presentation (with less efficient buffer
-- in loop).  Now 128 samples buffering with essentially no loss is
-- reached, provided you don't play a movie (or read your mail) on
-- your computer at the same time !
presentationFARM19 :: IO()
presentationFARM19 = do
  ch <- streamDB1ToChan
  r <- forkStreamToRef (makeStreamFromChan ch)
  let !f1 = feedbackDB1Ref2 r 800 0 2 1
      !f2 = feedbackDB1Ref r 5 3 4
      !f3 = feedbackDB1Ref3 r 0 5 6
  playAudioJack 128
      (spanStream (mapS (\x -> \y -> 0.3*x+0.6*y) ) id $
           spanStream avg f1 (f2 . f3))
                
