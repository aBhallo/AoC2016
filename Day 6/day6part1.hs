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
compute input = map (\x -> mostFreq $ map (!! x) input) [0..7]

mostFreq :: Ord a => [a] -> a
mostFreq list = head $ head $ sortBy (\x -> (\y -> compare (length y) (length x))) $ group $ sort list