module Main where

import Control.Monad
import Data.IORef
import Data.Foldable

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Field


main :: IO ()
main = do
    timeRef <- newIORef 0
    {- game main loop -}
    forever $ do
        time <- readIORef timeRef
        when (time `mod` 100000 == 0 ) $ print time
        modifyIORef timeRef (+1)
    return ()