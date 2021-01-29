module Field where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Random

type Coord = (Int, Int)

gameH, gameW, pileH , pileW :: Int
gameH = 22
gameW = 10
pileH = gameH + 2
pileW = gameW + 4

-- the origin of the game coord
origin :: Coord
origin = (0, 2)

-- Piece type, N for None, B for Pile Border
data PieceType = I | O | T | S | Z | J | L | N | B deriving (Eq)
data Direction = Spawn | DRight | Twice | DLeft deriving (Eq, Show, Enum)
data RotateDir = RLeft | RRight deriving (Eq)
data Piece = Piece { pType :: PieceType, pPos :: Coord, pDir :: Direction } deriving (Eq, Show)
type Pile = [[PieceType]]

data FieldST = FieldST { curPiece :: Piece, field :: Pile }
type FieldM = StateT FieldST IO

instance Show PieceType where
  show N = "  "
  show B = "##"
  show _ = "**"

(?+), (?-) :: Coord -> Coord -> Coord
(a, b) ?+ (c, d) = (a + c, b + d)
(a, b) ?- (c, d) = (a - c, b - d)

-- all piece types can be generated
pieceTypes :: [PieceType]
pieceTypes = [I, O, T, S, Z, J, L]

-- get the bounding Box of the piece
boundBox :: PieceType -> Direction -> Pile
boundBox I Spawn = [[N,N,N,N,N],[N,N,N,N,N],[N,I,I,I,I],[N,N,N,N,N],[N,N,N,N,N]]
boundBox I DRight = [[N,N,N,N,N],[N,N,I,N,N],[N,N,I,N,N],[N,N,I,N,N],[N,N,I,N,N]]
boundBox I Twice = [[N,N,N,N,N],[N,N,N,N,N],[I,I,I,I,N],[N,N,N,N,N],[N,N,N,N,N]]
boundBox I DLeft = [[N,I,N,N],[N,I,N,N],[N,I,N,N],[N,I,N,N]]

boundBox O Spawn = [[N,O,O],[N,O,O],[N,N,N]]
boundBox O DRight = [[N,N,N],[N,O,O],[N,O,O]]
boundBox O Twice = [[N,N,N],[O,O,N],[O,O,N]]
boundBox O DLeft = [[O,O,N],[O,O,N],[N,N,N]]

boundBox T Spawn = [[N,T,N],[T,T,T],[N,N,N]]
boundBox T DRight = [[N,T,N],[N,T,T],[N,T,N]]
boundBox T Twice = [[N,N,N],[T,T,T],[N,T,N]]
boundBox T DLeft = [[N,T,N],[T,T,N],[N,T,N]]

boundBox S Spawn = [[N,S,S],[S,S,N],[N,N,N]]
boundBox S DRight = [[N,S,N],[N,S,S],[N,N,S]]
boundBox S Twice = [[N,N,N],[N,S,S],[S,S,N]]
boundBox S DLeft = [[S,N,N],[S,S,N],[N,S,N]]

boundBox Z Spawn = [[Z,Z,N],[N,Z,Z],[N,N,N]]
boundBox Z DRight = [[N,N,Z],[N,Z,Z],[N,Z,N]]
boundBox Z Twice = [[N,N,N],[Z,Z,N],[N,Z,Z]]
boundBox Z DLeft = [[N,Z,N],[Z,Z,N],[Z,N,N]]

boundBox J Spawn = [[J,N,N],[J,J,J],[N,N,N]]
boundBox J DRight = [[N,J,J],[N,J,N],[N,J,N]]
boundBox J Twice = [[N,N,N],[J,J,J],[N,N,J]]
boundBox J DLeft = [[N,J,N],[N,J,N],[J,J,N]]

boundBox L Spawn = [[N,N,L],[L,L,L],[N,N,N]]
boundBox L DRight = [[N,L,N],[N,L,N],[N,L,L]]
boundBox L Twice = [[N,N,N],[L,L,L],[L,N,N]]
boundBox L DLeft = [[L,L,N],[N,L,N],[N,L,N]]

boundBox N _ = [[]]
-- end of boundBox

-- get the srs offset of each kind of rotation
getOffset :: PieceType -> Direction -> [Coord]
getOffset I d =
  case d of
    Spawn -> [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)]
    DRight -> [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)]
    Twice -> [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)]
    DLeft -> [(0, 1), (0, 1), (0, 1), (0, -1), (0, 2)]
getOffset O d =
  case d of
    Spawn -> [(0, 0)]
    DRight -> [(0, -1)]
    Twice -> [(-1, -1)]
    DLeft -> [(-1, 0)]
getOffset _ d =
  case d of
    Spawn -> replicate 5 (0, 0)
    DRight -> [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)]
    Twice -> replicate 5 (0, 0)
    DLeft -> [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)]

-- get the final rotate offset
srsOffset :: PieceType -> (Direction, Direction) -> [Coord]
srsOffset t (dFrom, dTo) =
  let from = getOffset t dFrom; to = getOffset t dTo
   in zipWith (?-) from to

emptyField :: Pile
emptyField =
  replicate gameH (wall ++ replicate gameW N ++ wall)
    ++ replicate 2 (replicate pileW B)
  where
    wall = [B, B]

-- place a piece
justPlace :: Piece -> Pile -> Pile
justPlace (Piece t (x, y) d) p =
  let box = boundBox t d
      l = length box
   in [ [ let p1 = p !! i !! j; p2 = box !! (i - x) !! (j - y)
           in if i >= x && i < x + l && j >= y && j < y + l
                && p1 == N
                then p2
                else p1
          | j <- [0 .. pileW -1]
        ]
        | i <- [0 .. pileH -1]
      ]

place :: FieldM ()
place = StateT $ \(FieldST (Piece t (x, y) d) p) ->
  let box = boundBox t d
      l = length box
      p' =
        [ [ let p1 = p !! i !! j; p2 = box !! (i - x) !! (j - y)
             in if i >= x && i < x + l && j >= y && j < y + l
                  && p1 == N
                  then p2
                  else p1
            | j <- [0 .. pileW -1]
          ]
          | i <- [0 .. pileH -1]
        ]
   in liftIO $ randVal pieceTypes
        >>= \randType -> return ((), FieldST (Piece randType (origin ?+ (0, 0)) Spawn) p')

-- checking if a piece is blocked after a move
isBlocked :: Piece -> Pile -> Bool
isBlocked (Piece t (x, y) d) p =
  or . join $
    let box = boundBox t d; l = length box
     in [ [ let p1 = p !! (i + x) !! (j + y); p2 = box !! i !! j
             in p1 /= N && p2 /= N
            | j <- [0 .. l -1]
          ]
          | i <- [0 .. l -1]
        ]

-- to check if a piece is blocked
checkBlocked :: Piece -> Pile -> Maybe Piece
checkBlocked p pile = if not $ isBlocked p pile then Just p else Nothing 

-- move a piece with a direction vector
-- if failed, it returns the original piece
movePiece :: Coord -> FieldM Bool
movePiece (dx, dy) = state $
  \(FieldST op@(Piece t (x, y) d) pile) ->
    let p' = Piece t (x + dx, y + dy) d
    in case checkBlocked p' pile of
      Nothing -> (False, FieldST op pile)
      _ -> (True, FieldST p' pile)

-- it ignores the result
movePiece_ :: Coord -> FieldM ()
movePiece_ x = void $ movePiece x 

rotate :: RotateDir -> Direction -> Direction
rotate RRight DLeft = Spawn
rotate RRight x = succ x
rotate RLeft Spawn = DLeft
rotate RLeft x = pred x

-- rotate a piece with a given direction
-- if failed, it returns the original piece
rotatePiece :: RotateDir -> FieldM ()
rotatePiece rdir = state $ 
  \f@(FieldST op@(Piece t c d) pile) ->
    let d' = rotate rdir d;
        offsetList = srsOffset t (d, d');
        alternatives = map (\offset -> checkBlocked (Piece t (c ?+ offset) d') pile) offsetList;
        res = foldr1 (<|>) alternatives
        in case res of 
          Nothing -> ((), FieldST op pile)
          Just p' -> ((), FieldST p' pile)

--showField 
getPile :: FieldM Pile
getPile = state $ \f@(FieldST p pile) -> 
  (justPlace p pile, f)

printList :: Show a => [[a]] -> IO ()
printList [] = return ()
printList (x : xs) = do
  mapM_ (putStr.(++" ").show) x
  putStr "\n"
  printList xs


-- tests
test1 = FieldST (Piece O (origin ?+ (0, 0)) Spawn) emptyField

test2 :: FieldM ()
test2 = do
  return ()
  -- movePiece (1,1)

printST :: FieldM () -> IO ()
printST s = do 
  (_, FieldST p pile) <- runStateT (s >> place) test1
  printList pile
