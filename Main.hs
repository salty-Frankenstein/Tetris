module Main where

import Control.Monad
import Control.Monad.State
import Data.IORef
import System.Exit
import Control.Concurrent
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Field

colorInit :: IO ()
colorInit = do
  Curses.startColor
  Curses.initPair (Curses.Pair 1) CursesH.cyan CursesH.cyan
  Curses.initPair (Curses.Pair 2) CursesH.blue CursesH.blue
  Curses.initPair (Curses.Pair 3) CursesH.white CursesH.white
  Curses.initPair (Curses.Pair 4) CursesH.yellow CursesH.yellow
  Curses.initPair (Curses.Pair 5) CursesH.green CursesH.green
  Curses.initPair (Curses.Pair 6) CursesH.magenta CursesH.magenta
  Curses.initPair (Curses.Pair 7) CursesH.red CursesH.red
  Curses.initPair (Curses.Pair 8) CursesH.red Curses.defaultBackground
  Curses.initPair (Curses.Pair 9) CursesH.black CursesH.black

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
      showLine x i
      drawField' xs (i + 1)
    
    drawPiece :: PieceType -> Coord -> IO ()
    drawPiece p (x, y) = do
      let color p = fromEnum p + 1
      Curses.attrSet Curses.attr0 (Curses.Pair $ color p)
      Curses.mvWAddStr Curses.stdScr x y (show p)

    showLine :: [PieceType] -> Int -> IO ()
    showLine l i = do
      sequence_ $ zipWith drawPiece l (repeat i `zip` [0,2..])

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
  liftIO $ Curses.attrSet Curses.attr0 (Curses.Pair 8)
  liftIO $ Curses.mvWAddStr Curses.stdScr 0 0 (show time)

  case gameST of
    GameExit -> liftIO exitSuccess
    _ -> when (time `mod` 10000 == 0) $ do
      c <- liftIO $ readIORef keyRef
      case c of
        Curses.KeyChar 'q' -> 
          liftIO $ writeIORef gameSTRef GameExit
        Curses.KeyLeft -> movePiece_ (0, -1)
        Curses.KeyRight -> movePiece_ (0, 1)
        Curses.KeyChar 'z' -> rotatePiece RLeft
        Curses.KeyChar 'x' -> rotatePiece RRight
        _ -> return ()

      liftIO (clearKey keyRef)
      
      {- falling -}
      when (time `mod` 20000 == 0) $ do
        res <- movePiece (1, 0)
        unless res place

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
  colorInit

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