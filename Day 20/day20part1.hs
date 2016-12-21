import Data.List

main = do  
        contents <- readFile "day20input.txt"
        let result = compute $ lines contents
        print result
  
compute :: [String] -> Int  
compute input = findSmallestGap sorted
    where sorted = sort $ parseInput input
    
findSmallestGap :: [(Int, Int)] -> Int
findSmallestGap ((_, b):(c, d):rest)
    | b + 1 < c = b + 1
    | otherwise = findSmallestGap ((c,d):rest)

parseInput :: [String] -> [(Int, Int)]    
parseInput input = map ((\(a, b) -> (read a, read $ tail b)) . break (=='-')) input