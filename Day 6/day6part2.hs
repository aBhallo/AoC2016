import System.IO  
import Data.List

main = do  
        handle <- openFile "day6input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   
   
compute :: [String] -> String
compute input = map (\x -> leastFreq $ map (!! x) input) [0..7]

leastFreq :: Ord a => [a] -> a
leastFreq list = fst $ last $ sortBy valueOrder $ map (\x -> (head x, length x)) $ group $ sort list

valueOrder :: Ord a => (s, a) -> (t, a) -> Ordering
valueOrder (_, v1) (_, v2) = compare v2 v1