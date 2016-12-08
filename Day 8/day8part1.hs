import System.IO
import Data.List
import Data.Char

main = do  
        contents <- readFile "day8input.txt"
        let result = compute $ lines contents
        print result
   
type Screen = [[Bool]]

screenWidth :: Int
screenWidth = 50

screenHeight :: Int
screenHeight = 6

initScreen :: Screen
initScreen = replicate screenWidth $ replicate screenHeight False
   
compute input = numLit $ foldl executeInstr initScreen input

executeInstr :: Screen -> String -> Screen
executeInstr screen instr 
    | "rect" `isPrefixOf` instr = rect screen (read w) (read $ tail h)
    | "rotate row" `isPrefixOf` instr = rotateRow screen (read a) (read n)
    | "rotate column" `isPrefixOf` instr = rotateCol screen (read a) (read n)
    where (w, h) = span (/= 'x') rest
          [a,_,n] = words $ rest
          rest = snd $ span (not . isDigit) instr

rect :: Screen -> Int -> Int -> Screen
rect screen w h = map (\x -> replicate h True ++ drop h x) (take w screen) ++ drop w screen

rotateCol :: Screen -> Int -> Int -> Screen
rotateCol screen y n = take y screen ++ [rotatedCol] ++ drop (y + 1) screen
    where rotatedCol = take height $ drop (abs $ height - n) $ cycle $ screen !! y
          height = length $ head screen

rotateRow :: Screen -> Int -> Int -> Screen
rotateRow screen x n = rotateScreen270 $ rotateCol (rotateScreen90 screen) x n

rotateScreen270 :: Screen -> Screen
rotateScreen270 = (!! 3) . iterate rotateScreen90 

rotateScreen90 :: Screen -> Screen
rotateScreen90 screen = map (\x -> map (!! x) screen) [0..height - 1]
    where height = length $ head screen
       
numLit :: Screen -> Int
numLit screen = sum $ map length $ map (filter (\x -> x)) screen