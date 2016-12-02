import System.IO  
import Data.List
import Data.Char
import Data.Maybe

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
type Keypad = [[Maybe Char]]

keypad :: Keypad
keypad = [[Nothing, Nothing, Just '5', Nothing, Nothing],
          [Nothing, Just '2', Just '6', Just 'A', Nothing],
          [Just '1', Just '3', Just '7', Just 'B', Just 'D'],
          [Nothing, Just '4', Just '8', Just 'C', Nothing],
          [Nothing, Nothing, Just '9', Nothing, Nothing]]
        

startPoint :: Coord
startPoint = (0,2)

compute :: [String] -> String
compute input = coordsToIntSeq $ applyInstructions startPoint input

coordsToIntSeq :: [Coord] -> String
coordsToIntSeq = map (\(x, y) -> fromJust (keypad !! x !! y))

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
isValid (x,y)
    | x < 0 || x > 4 || y < 0 || y > 4 = False
    | otherwise = isJust (keypad !! x !! y)

applyDirection :: Dir -> Coord -> Coord
applyDirection L (x,y) = (x - 1, y)
applyDirection D (x,y) = (x, y + 1)
applyDirection R (x,y) = (x + 1, y)
applyDirection U (x,y) = (x, y - 1)
