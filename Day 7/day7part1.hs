import System.IO  
import Data.List.Split

main = do  
        handle <- openFile "day7input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   
   
compute :: [String] -> Int
compute input = length $ filter supportsTLS $ map splitHypernets input

supportsTLS :: [String] -> Bool
supportsTLS ip = not (or hyper) && (or super)
    where (super, hyper) = split2 $ map isABBA ip

isABBA :: String -> Bool
isABBA (s:t:u:v:rest)
    | s == v && t == u && s /= t = True
    | otherwise = isABBA $ (t:u:v:rest)
isABBA _ = False

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 [x] = ([x], [])
split2 (x:y:rest) = (x:xs, y:ys)
    where (xs, ys) = split2 rest
    
splitHypernets :: String -> [String]
splitHypernets = splitWhen $ \x -> x == '[' || x == ']'