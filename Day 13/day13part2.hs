import qualified AStar
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Maybe
import Numeric

main = print compute

type Coord = (Int, Int)

designersNumber :: Int
designersNumber = 1350

start :: Coord
start = (1, 1)

compute = length $ nub $ concat $ take 51 paths
    where paths = iterate (nub . concat . map nextStates) [start]

isWalkable :: Coord -> Bool
isWalkable (x, y) = x >= 0 && y >= 0 && even numOnes
    where numOnes = length $ filter (== '1') $ showIntAtBase 2 intToDigit sum ""
          sum = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + designersNumber
    
nextStates :: Coord -> [Coord]
nextStates (x, y) = filter isWalkable [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]