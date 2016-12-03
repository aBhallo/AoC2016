import System.IO  
import Data.List
import Data.Char

main = do  
        handle <- openFile "day3input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   
   
compute :: [String] -> Int
compute input = length $ filter validTriangle tripleList
    where tripleList = collect3 $ map read input 
   
validTriangle :: [Int] -> Bool    
validTriangle lengths = sumGreater $ sort lengths

sumGreater :: [Int] -> Bool
sumGreater (a:b:c:[]) = a + b > c

collect3 :: [Int] -> [[Int]]
collect3 [] = []
collect3 list = take 3 list : collect3 (drop 3 list)