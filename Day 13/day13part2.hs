import qualified AStar
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Numeric

main = print compute

type Coord = (Int, Int)    -- (x co-ord, y co-ord)

designersNumber :: Int
designersNumber = 1350

start :: Coord
start = (1, 1)

maxDepth :: Int
maxDepth = 50

compute = length validPaths
    where validPaths = filter (\(c,_) -> c <= maxDepth) $ map fromJust paths
          paths = filter isJust $ map search possibleGoals
          possibleGoals = [(x, y) | x <- [0..maxDepth], y <- [0..maxDepth], x + y <= maxDepth + 1, isWalkable (x, y)]
          search end = AStar.search start (== end) nextStates (heuristic end)
          heuristic (x', y') (x, y) = (y' - y) + (x' - x)

isWalkable :: Coord -> Bool
isWalkable (x, y) = x >= 0 && y >= 0 && even numOnes
    where numOnes = length $ filter (== '1') $ showIntAtBase 2 intToDigit sum ""
          sum = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + designersNumber
    
nextStates :: Coord -> [(Coord, Int)]
nextStates (x, y) = map (\coord -> (coord, 1)) $ filter isWalkable possibleCoords
    where possibleCoords = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]