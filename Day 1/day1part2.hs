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

compute input = manhattanDistance target startCoord
    where filteredList = map (filter $ \c -> c /= ',') input
          visited = applyExecution startPos filteredList
          target = firstDuplicate [] $ map posToCoord visited
          
applyExecution :: Pos -> [String] -> [Pos]
applyExecution startPos [] = []
applyExecution startPos (s:str) = visited ++ applyExecution nextPoint str
     where visited = executeInstruction startPos s
           nextPoint = last(visited)
           
posToCoord :: Pos -> Coord
posToCoord ((x,y),_) = (x,y)           
           
firstDuplicate :: [Coord] -> [Coord] -> Coord
firstDuplicate beginning (y:ys)
    | y `isIn` beginning = y 
    | otherwise          = firstDuplicate (beginning ++ [y]) ys
firstDuplicate _ _ = error "No duplicates"

isIn :: Coord -> [Coord] -> Bool
coord `isIn` [] = False
coord `isIn` (c:cs) 
    | coord == c = True 
    | otherwise  = coord `isIn` cs

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

executeInstruction :: Pos -> String -> [Pos]
executeInstruction point str = rotateAndMove point (read $ [head str]) (read $ tail str)
          
rotateAndMove :: Pos -> Rotation -> Steps -> [Pos]
rotateAndMove pos rotation steps = visited
    where (visited) = tail $ moveNSteps rotatedPos steps
          rotatedPos = rotate pos rotation
          
rotate :: Pos -> Rotation -> Pos
rotate ((x,y),N) o = if o == L then ((x,y),W) else ((x,y),E)
rotate ((x,y),E) o = if o == L then ((x,y),N) else ((x,y),S)
rotate ((x,y),S) o = if o == L then ((x,y),E) else ((x,y),W)
rotate ((x,y),W) o = if o == L then ((x,y),S) else ((x,y),N)

moveNSteps :: Pos -> Steps -> [Pos]
moveNSteps pos 0 = [pos]
moveNSteps pos n = pos : (moveNSteps (newPos) (n-1))
    where newPos = moveOneStep pos

moveOneStep :: Pos -> Pos
moveOneStep ((x,y),N) = ((x, y - 1), N)
moveOneStep ((x,y),E) = ((x + 1, y), E)
moveOneStep ((x,y),S) = ((x, y + 1), S)
moveOneStep ((x,y),W) = ((x - 1, y), W)