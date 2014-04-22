import Data.Array
import Data.List (intersperse)

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

directions :: [Direction]
directions = [north, (north . east), east, (south . east),
              south, (south . west), west, (north. west)]
             
isInBounds :: Boundary
isInBounds (row, col) = and [row >= 0, row < 8, col >= 0, col < 8]
 
-- Starting Coord is not included in the walk
walkDirection :: Boundary -> Coord -> Direction -> [Coord]
walkDirection bound crd dir = takeWhile bound $ tail $ iterate dir crd

--

data Player = White | Black deriving(Show, Eq)
data Square = Empty | Marked Player deriving(Show, Eq)
data Board = Board { grid :: (Array Coord Square) 
                   , player :: Player 
                   } deriving(Show)

initialBoard :: Board
initialBoard = Board (emptyBoard // initialMarks) Black
  where
    emptyBoard = array ((0,0), (7,7)) [((x,y), Empty) | x <- [0..7], y <- [0..7]]
    initialMarks = [((3,3), Marked White), 
                    ((4,4), Marked White), 
                    ((3,4), Marked Black), 
                    ((4,3), Marked Black)]

flipPlayer :: Player -> Player
nflipPlayer White = Black
flipPlayer Black = White

flipSquare :: Square -> Square
flipSquare (Marked Black) = Marked White
flipSquare (Marked White) = Marked Black
flipSquare Empty = Empty

flipCoord :: Board -> Coord -> Board
flipCoord (Board g p) coord = Board (g // [(coord, flipSquare $ g ! coord)]) p

makeMove :: Board -> Coord -> Board
makeMove board@(Board g p) move
  | isLegal board move = foldr flipAll markedBoard (flippableWalks board move)
  | otherwise = board
  where
    markedBoard = (Board (g // [(move, Marked p)]) (flipPlayer p))
    flipAll coords currBoard = foldr (flip flipCoord) currBoard coords

flippableWalks :: Board -> Coord -> [[Coord]]
flippableWalks (Board g p) move = map farthestFlippable directions
  where
    farthestFlippable direction = let cs = walkDirection flippable move direction
                                      sqs = map (g !) cs
                                  in case cs of [] -> []
                                                [x] -> []
                                                (x:xs)
                                                    | any (== (Marked p)) sqs -> takeWhile (\x -> (g ! x) /= (Marked p)) cs
                                                    | otherwise -> []


    flippable coord = (isInBounds coord) &&
                      ((g ! coord) /= Empty)

isEmptySquare :: Square -> Bool
isEmptySquare Empty = True
isEmptySquare _ = False

isMarkedBy :: Player -> Square -> Bool
isMarkedBy p (Marked m) = p == m
isMarkedBy _ _ = False

isLegal :: Board -> Coord -> Bool
isLegal board@(Board g p) move = ((g ! move) == Empty) &&
                                         (any (not . null) $ flippableWalks board move)

legalMoves :: Board -> [Coord]
legalMoves board = foldr (\c acc -> if isLegal board c then c:acc else acc) [] allCoords
  where
    allCoords = [(x,y) | x <- [0..7], y <- [0..7]]

-- Returns `Nothing` if game isn't over. `Just <WINNER>` if the game is over
gameOver :: Board -> Maybe Player
gameOver b@(Board g p)
    | (null $ legalMoves b) && 
      (null $ legalMoves $ b {player = (flipPlayer p)}) = Just White
    -- TODO: Determine winner
    | otherwise = Nothing

-- The code is filthy, but what print function isn't?
printBoard :: Board -> IO ()
printBoard (Board g _) = do putStrLn $ ("  "++) $ intersperse ' ' ['A'..'H']
                            mapM_ (putStrLn . stringRow) allCoords
                            putStrLn $ ("  "++) $ intersperse ' ' ['A'..'H']
  where
    allCoords = [[(x,y) | y <- [0..7]] | x <- [0..7]]
    stringRow row@((x,_):_) = (show x) ++ " " ++ (foldl (\acc x -> acc ++ squareToChar (g ! x) ++ " ") [] row) ++ "" ++ (show x)
    squareToChar Empty = "-"
    squareToChar (Marked White) = "O"
    squareToChar (Marked Black) = "*"

-- TODO:
-- Edax string to Board
-- Board to Edax string
-- Board to pretty string (instead of print)
-- Make numbers 1-8 instead of 0-7
