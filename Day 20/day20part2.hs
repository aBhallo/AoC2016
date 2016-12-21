import Data.List

main = do  
        contents <- readFile "day20input.txt"
        let result = compute $ lines contents
        print result

maxVal :: Int
maxVal = 4294967295
  
compute :: [String] -> Int  
compute input = findNumGaps sorted
    where sorted = (fixList . sort . parseInput) input
 
fixList :: [(Int, Int)] -> [(Int, Int)]
fixList [] = []
fixList ((a, b):rest) = (a, b) : fixList newList
    where newList = dropWhile (\(_, c) -> (b > c)) rest
    
findNumGaps :: [(Int, Int)] -> Int
findNumGaps [(a, b)]
    | b < maxVal = maxVal - b
    | otherwise = 0
findNumGaps ((_, b):(c, d):rest)
    | b + 1 < c = c - (b + 1) + next
    | otherwise = next
    where next = findNumGaps ((c,d):rest)

parseInput :: [String] -> [(Int, Int)]    
parseInput input = map ((\(a, b) -> (read a, read $ tail b)) . break (=='-')) input