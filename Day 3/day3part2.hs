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
    where tripleList = collect3 3 $ map read input 
   
validTriangle :: [Int] -> Bool    
validTriangle lengths = sumGreater $ sort lengths

sumGreater :: [Int] -> Bool
sumGreater (a:b:c:[]) = a + b > c

collect3 :: Int -> [Int] -> [[Int]]
collect3 _ [] = []
collect3 0 list = collect3 3 $ drop 6 list
collect3 n list = hd : tl
    where hd = ([list !! 0] ++ [list !! 3] ++ [list !! 6])
          tl = if endReached then [] else collect3 (n-1) $ tail list
          endReached = length list == 7