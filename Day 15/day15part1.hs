import Data.Char
import Data.List
import Data.Maybe

main = do  
        contents <- readFile "day15input.txt"
        let result = compute $ map words $ lines contents
        print result

type Disk = (Int, Int)
     
compute :: [[String]] -> Int     
compute input = findValidTime $ parse input

findValidTime :: [Disk] -> Int
findValidTime disks = fromJust $ findIndex (id) $ map (validTime disks) [0..]

validTime :: [Disk] -> Int -> Bool
validTime disks time = and $ map (\i -> isReached (time + i) $ disks !! (i - 1)) [1..length disks]
    where isReached rotations (numPos, pos) = (rotations + pos) `mod` numPos == 0  

parse :: [[String]] -> [Disk]
parse = map (tuple . (map read . filter (\x -> not (x == "" || x == "0")) . drop 2 . map (filter isDigit)))
    where tuple [a,b] = (a,b)