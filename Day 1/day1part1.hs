import System.IO  
import Data.List
import Data.Char

main = do  
        handle <- openFile "day1input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   
        
data Rotation = L | R deriving (Show, Read, Eq)
data Orient = N | E | S | W deriving (Show, Read, Eq)
type Steps = Int
type Instruction = (Rotation, Steps)
type Coord = (Int, Int)
type Pos = (Coord, Orient)

startCoord :: Coord
startCoord = (0,0)

startPos :: Pos
startPos = (startCoord,N)

compute input = manhattanDistance finalCoord startCoord
    where filteredList = map (filter $ \c -> c /= ',') input
          (finalCoord, _) = foldl executeInstruction startPos filteredList
          
manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

executeInstruction :: Pos -> String -> Pos
executeInstruction point str = rotateAndMove point (read $ [head str]) (read $ tail str)
          
rotateAndMove :: Pos -> Rotation -> Steps -> Pos
rotateAndMove pos rotation steps = movedPos
    where movedPos = moveNSteps rotatedPos steps
          rotatedPos = rotate pos rotation
          
rotate :: Pos -> Rotation -> Pos
rotate ((x,y),N) o = if o == L then ((x,y),W) else ((x,y),E)
rotate ((x,y),E) o = if o == L then ((x,y),N) else ((x,y),S)
rotate ((x,y),S) o = if o == L then ((x,y),E) else ((x,y),W)
rotate ((x,y),W) o = if o == L then ((x,y),S) else ((x,y),N)

moveNSteps :: Pos -> Steps -> Pos
moveNSteps pos n = foldr (.) id (replicate n moveOneStep) pos   

moveOneStep :: Pos -> Pos
moveOneStep ((x,y),N) = ((x, y - 1), N)
moveOneStep ((x,y),E) = ((x + 1, y), E)
moveOneStep ((x,y),S) = ((x, y + 1), S)
moveOneStep ((x,y),W) = ((x - 1, y), W)