module Field where

import Control.Monad

type Coord = (Int, Int)

-- Piece type, N for None
data PieceType = I | O | T | S | Z | J | L | N deriving (Eq, Show)
data Direction = Spawn | DLeft | DRight | Twice deriving (Eq)
data RotateDir = RLeft | RRight deriving (Eq)
data Piece = Piece { pType :: PieceType, pPos :: Coord, pDir :: Direction }
type Pile = [[PieceType]]

diff :: Coord -> Coord -> Coord
diff (a, b) (c, d) = (a - c, b - d)

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
getOffset I d = case d of
    Spawn -> [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)]
    DRight -> [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)]
    Twice -> [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)]
    DLeft -> [(0, 1), (0, 1), (0, 1), (0,-1), (0, 2)]
getOffset O d = case d of
    Spawn -> [(0, 0)]
    DRight -> [(0, -1)]
    Twice -> [(-1, -1)]
    DLeft -> [(-1, 0)]
getOffset _ d = case d of
    Spawn -> replicate 5 (0, 0)
    DRight -> [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)]
    Twice -> replicate 5 (0, 0)
    DLeft -> [(0, 0), (-1, 0), (-1,-1), (0, 2), (-1, 2)]

-- get the final rotate offset
srsOffset :: PieceType -> (Direction, Direction) -> [Coord]
srsOffset t (dFrom, dTo) = 
    let from = getOffset t dFrom; to = getOffset t dTo in
        zipWith diff from to

emptyField :: Pile
emptyField = replicate 22 $ replicate 10 N 


-- place a piece
place :: Piece -> Pile -> Pile
place (Piece t (x, y) d) p = 
    let box = boundBox t d; l = length box in
        [[ let p1 = p!!i!!j; p2 = box!!(i-x)!!(j-y) in
            if i >= x && i < x+l && j >= y && j < y+l then
                if p1 == N then p2 else p1
            else p1
            | j <- [0..9]] | i <- [0..21]]

-- checking if a piece is blocked after a move
isBlocked :: Piece -> Pile -> Bool
isBlocked (Piece t (x, y) d) p = or . join $
    let box = boundBox t d; l = length box in
        [[ let p1 = p!!(i+x)!!(j+y); p2 = box!!i!!j in
            p1 /= N && p2 /= N
            | j <- [0..l-1]] | i <- [0..l-1]]

-- move a piece with a direction vector
-- if failed, it returns the original piece
movePiece :: Piece -> Coord -> Pile -> Piece
movePiece op@(Piece t (x, y) d) (dx, dy) p = 
    let p' = Piece t (x+dx, y+dy) d in
        if not $ isBlocked p' p then p' else op 

-- rotate a piece with a given direction
-- if failed, it returns the original piece
-- rotatePiece :: Piece -> RotateDir -> Piece

--showField 

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x 
    printList xs

