import Control.Monad
-- import System.Win32.Process
import System.Posix.Unistd
import Control.Concurrent

import Data.IORef
import Data.Foldable
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

-- import Keyboard
-- import Time

-- foreverPrint :: Char -> IO ()
-- foreverPrint c = forever $ do
--     print c
--     sleep 1000

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
    CursesH.start
    aRef <- newIORef 0
    bRef <- newIORef "a"
    forkIO $ forever $ do
        a <- readIORef aRef
        b <- readIORef bRef
        when (a `mod` 100000 == 0 ) $ do
            Curses.mvWAddStr Curses.stdScr 0 0 (show a)
            Curses.mvWAddStr Curses.stdScr 1 0 (show b)
            Curses.refresh

        modifyIORef aRef (+1)
    
    forkIO $ forever $ do
        c <- Curses.getCh 
        writeIORef bRef (show c)

    -- forkIO $ forever $ do
    --     b <- readIORef bRef
    --     when (b `mod` 100000 == 5 ) $ print b
    --     modifyIORef bRef (+1)
    
    threadDelay 10000000
    -- sleep 1
    return ()
-- main :: IO ()
-- main = do
--     m <- newMVar '0'
--     forkIO $ forever $ do
--         c <- getChar
--         putMVar m c
--     forkIO $ forever $ do
--         c <- takeMVar m
--         print c
--         sleep 2
    
--     return ()
    --forever $ return ()

