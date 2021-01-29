module Main where

import Control.Monad
import Control.Monad.State
import Data.IORef
-- import Data.Foldable
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

clearKey :: IORef Curses.Key -> IO ()
clearKey keyRef = writeIORef keyRef $ Curses.KeyChar ' ' 

data GameST = GameStart | GameRun | GameOver | GameExit
type Time = Integer 

initGame :: FieldST
initGame = test1

{- game main loop, with global variables -}
{- it should react every timetick -}
mainLoop :: IORef GameST -> IORef Time -> IORef Curses.Key -> FieldM ()
mainLoop gameSTRef timeRef keyRef = forever $ do
  gameST <- liftIO $ readIORef gameSTRef
  liftIO $ modifyIORef timeRef (+1)
  time <- liftIO $ readIORef timeRef
  liftIO $ Curses.mvWAddStr Curses.stdScr 0 0 (show time)

  case gameST of
    GameExit -> liftIO exitSuccess
    _ -> when (time `mod` 10000 == 0) $ do
      c <- liftIO $ readIORef keyRef
      case c of
        Curses.KeyChar 'q' -> 
          liftIO $ writeIORef gameSTRef GameExit
        Curses.KeyLeft -> movePiece (0, -1)
        Curses.KeyRight -> movePiece (0, 1)
        Curses.KeyChar 'z' -> rotatePiece RLeft
        Curses.KeyChar 'x' -> rotatePiece RRight
        _ -> return ()

      liftIO (clearKey keyRef)
      
      {- falling -}
      when (time `mod` 100000 == 0) $ movePiece (1, 0)

      getPile >>= \p -> liftIO $ drawField p
      liftIO Curses.refresh

{- keyboard event -}
keyEvent :: IORef Curses.Key -> IO ()
keyEvent keyRef = do
  key <- Curses.getCh
  writeIORef keyRef key
  return ()

main :: IO ()
main = do
  CursesH.start

  {- global variables -}
  timeRef <- newIORef 0
  gameSTRef <- newIORef GameStart
  keyRef <- newIORef $ Curses.KeyChar ' '

  {- game main loop -}
  forkIO . forever $ keyEvent keyRef
  forkIO $ evalStateT (mainLoop gameSTRef timeRef keyRef) initGame

  {- main: wait for GameExit -}
  forever $ do
    gameST <- readIORef gameSTRef
    case gameST of
      GameExit -> Curses.endWin `seq` exitSuccess
      _ -> threadDelay 1000
  
  return ()