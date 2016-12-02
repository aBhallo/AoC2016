import System.IO  
import Data.List
import Data.Char

-- Day 2 Part 1

main = do  
        handle <- openFile "day2input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   

data Dir = L | D | R | U deriving (Read, Show)
type Instruction = [Dir]
type Coord = (Int, Int)
type Keypad = [[Int]]

keypad :: Keypad
keypad = [[1,4,7],
          [2,5,8],
          [3,6,9]]

startPoint :: Coord
startPoint = (1,1)

compute :: [String] -> String
compute input = coordsToIntSeq $ applyInstructions startPoint input

coordsToIntSeq :: [Coord] -> String
coordsToIntSeq = map (\(x, y) -> intToDigit (keypad !! x !! y))

applyInstructions :: Coord -> [String] -> [Coord]
applyInstructions start [] = []
applyInstructions start (d:ds) = newPoint : applyInstructions newPoint ds
    where newPoint = applyDirections start d
    
applyDirections :: Coord -> String -> Coord
applyDirections start [] = start
applyDirections start (d:ds) = applyDirections validPoint ds
    where newPoint = applyDirection (read [d]) start
          validPoint = if isValid newPoint then newPoint else start

isValid :: Coord -> Bool
isValid (x,y) = not $ x < 0 || x > 2 || y < 0 || y > 2

applyDirection :: Dir -> Coord -> Coord
applyDirection L (x,y) = (x - 1, y)
applyDirection D (x,y) = (x, y + 1)
applyDirection R (x,y) = (x + 1, y)
applyDirection U (x,y) = (x, y - 1)
