import Control.Monad
import System.Win32.Process
import Control.Concurrent

import Keyboard
import Time

foreverPrint :: Char -> IO ()
foreverPrint c = forever $ do
    print c
    sleep 1000
{-
main :: IO ()
main = do
    forkIO $ foreverPrint 'a'
    forever $ do 
        c <- input
        putChar c
    return ()
-}

main :: IO ()
main = do
    m <- newMVar '0'
    forkIO $ forever $ do
        c <- getChar
        putMVar m c
    forkIO $ forever $ do
        c <- takeMVar m
        print c
        sleep 2
    
    return ()
    --forever $ return ()

