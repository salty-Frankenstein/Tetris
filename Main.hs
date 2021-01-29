module Main where

import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Foldable
import System.Exit
import Control.Concurrent
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Field

-- draw s = do
--   (h, w) <- Curses.scrSize
--   CursesH.gotoTop
--   CursesH.drawLine w s
--   Curses.refresh

drawField :: Pile -> IO ()
drawField p = do
  drawField' p 1
  Curses.refresh
  where
    drawField' [] _ = return ()
    drawField' (x : xs) i = do
      let str = concatMap ((++ " ") . show) x
      Curses.mvWAddStr Curses.stdScr i 0 str
      drawField' xs (i + 1)

data GameST = GameStart | GameRun | GameOver | GameExit

initGame :: FieldST
initGame = test1

{- game main loop, with global variables -}
mainLoop :: IORef GameST -> IORef Integer -> FieldM ()
mainLoop gameSTRef timeRef = forever $ do
  gameST <- liftIO $ readIORef gameSTRef
  case gameST of
    GameExit -> liftIO exitSuccess
    _ -> do
      c <- liftIO Curses.getCh
      case c of
        Curses.KeyChar 'q' -> 
          liftIO $ writeIORef gameSTRef GameExit
        Curses.KeyLeft -> movePiece (0, -1)
        Curses.KeyRight -> movePiece (0, 1)
        _ -> return ()

      time <- liftIO $ readIORef timeRef
      when (time `div` 100000 `mod` 2 == 0) $ movePiece (1, 0)

      p <- getPile
      liftIO $ drawField p
      liftIO $ Curses.mvWAddStr Curses.stdScr 0 0 (show time)
      liftIO Curses.refresh



main :: IO ()
main = do
  CursesH.start

  {- global variables -}
  timeRef <- newIORef 0
  gameSTRef <- newIORef GameStart

  {- game main loop -}
  -- modifyIORef timeRef (+1)
  forkIO $ forever $ do
    time <- liftIO $ readIORef timeRef
    when (time `mod` 100000 == 0) $ do
      Curses.refresh
    modifyIORef timeRef (+1)

  forkIO $ evalStateT (mainLoop gameSTRef timeRef) initGame
  threadDelay 10000000
  
  return ()