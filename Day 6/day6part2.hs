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
leastFreq list = head $ head $ sortBy (\x -> (\y -> compare (length x) (length y))) $ group $ sort list