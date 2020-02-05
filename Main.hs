module Main where

import Sound.JACK.Audio
import qualified Sound.JACK as Jack
import qualified Control.Monad.Trans.Class as Trans
import System.Environment (getProgName)

import Control.Concurrent (forkIO)
    
    
import Audio
import MonadStream
import MonadStreamReference
import MonadReference
import IOStream
import TimedMonad    
import MultiTimedMonad    

import DB1

    
main = -- (run . execStream . (fmap (const ())) . tryMusicIOSTempo) 0
    -- (run . execStream . (fmap (const ())) . trySTIO) 0
    -- echoStdIORef
    echo2StdIO
    -- echoStdIO

{-

      -- FARM presentation : no need to try without a DB1 organ controler which probably you have not (who does ?)
main = do
    presentationFARM19
    c <- getChar
    return ()
-}
