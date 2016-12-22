import Data.List
import Data.Maybe

main = do  
        contents <- readFile "day21input.txt"
        let result = compute $ map words $ lines contents
        print result
    
compute :: [[String]] -> String    
compute input = foldl performAction "abcdefgh" input

performAction :: String -> [String] -> String
performAction str ("swap":"position":x:_:_:y:[]) = prefix ++ [str !! y'] ++ midfix ++ [str !! x'] ++ suffix
    where [x', y'] = sort [read x :: Int, read y :: Int]
          prefix = take x' str
          midfix = drop (x'+1) $ take (y') str
          suffix = drop (y' + 1) str
          
performAction str ("swap":"letter":x:_:_:y:[]) = performAction str ["swap", "position", show x', "","", show y']
    where (x', y') = (fromJust $ elemIndex (x !! 0) str, fromJust $ elemIndex (y !! 0) str)
    
performAction str ("rotate":dir:x:_:[]) = take len $ drop n $ cycle str
    where n = if dir == "left" then x' `mod` len else len - x' `mod` len
          x' = read x
          len = length str
          
performAction str ("rotate":"based":_:_:_:_:x:[]) = performAction str ["rotate", "right", show x', ""]
    where x' = if x'' > 3 then x'' + 2 else x'' + 1
          x'' = fromJust $ elemIndex (x !! 0) str

performAction str ("reverse":_:x:_:y:[]) = prefix ++ midfix ++ suffix
    where [x', y'] = sort [read x :: Int, read y :: Int]
          prefix = take x' str
          midfix = reverse $ drop x' $ take (y' + 1) str
          suffix = drop (y' + 1) str
          
performAction str ("move":_:x:_:_:y:[]) = prefix ++ [letter] ++ suffix
    where (x', y') = (read x :: Int, read y :: Int)
          (prefix, suffix) = splitAt y' $ delete letter str
          letter = str !! x'