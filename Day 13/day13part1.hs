import qualified AStar
import Data.Bits
import Data.Char
import Numeric

main = print compute

type Coord = (Int, Int)    -- (x co-ord, y co-ord)

goal :: Coord
goal = (31, 39)

designersNumber :: Int
designersNumber = 1350

start :: Coord
start = (1, 1)

compute :: Int
compute = cost
    where Just (cost, path) = AStar.search start isGoalState nextStates heuristic
 
isWalkable :: Coord -> Bool
isWalkable (x, y) = even numOnes
    where numOnes = length $ filter (== '1') $ showIntAtBase 2 intToDigit sum ""
          sum = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + designersNumber
    
nextStates :: Coord -> [(Coord, Int)]
nextStates (x, y) = map (\coord -> (coord, 1)) $ filter isWalkable possibleCoords
    where possibleCoords = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isGoalState :: Coord -> Bool
isGoalState = (== goal)

heuristic :: Coord -> Int
heuristic (x, y) = (y' - y) + (x' - x)
    where (x', y') = goal