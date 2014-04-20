import Data.Array

type Coord = (Int, Int)
type Direction = Coord -> Coord
type Boundary = Coord -> Bool
 
north :: Direction
north (row, col) = (row - 1, col)
 
south :: Direction
south (row, col) = (row + 1, col)
 
east :: Direction
east (row, col) = (row, col + 1)
 
west :: Direction
west (row, col) = (row, col - 1)
 
isInBounds :: Boundary
isInBounds (row, col) = and [row >= 0, row < 8, col >= 0, col < 8]
 
-- Starting Coord is not included in the walk
walkDirection :: Boundary -> Direction -> Coord -> [Coord]
walkDirection bound dir crd = takeWhile bound $ tail $ iterate dir crd

--

data Player = White | Black deriving(Show)
data Square = Empty | Marked Player deriving(Show)
data Board = Board (Array Coord Square) Player deriving(Show)

initialBoard :: Board
initialBoard = Board (emptyBoard // initialMarks) White
  where
    emptyBoard = array ((0,0), (7,7)) [((x,y), Empty) | x <- [0..7], y <- [0..7]]
    initialMarks = [((3,3), Marked White), 
                    ((4,4), Marked White), 
                    ((3,4), Marked Black), 
                    ((4,3), Marked Black)]

flipPlayer :: Player -> Player
flipPlayer White = Black
flipPlayer Black = White

flipSquare :: Square -> Square
flipSquare Marked Black = Marked White
flipSquare Marked White = Marked Black

makeMove :: Board -> Coord -> Maybe Board
makeMove board@(Board _ player) move
  | isLegal board move = Just $ board // [(move, Marked player)]
  | otherwise = Nothing

isLegal :: Board -> Coord -> Bool
isLegal board move = True --!!

