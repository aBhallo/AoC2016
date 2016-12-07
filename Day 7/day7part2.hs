import System.IO  
import Data.List
import Data.List.Split

main = do  
        handle <- openFile "day7input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   
   
compute :: [String] -> Int
compute input = length $ filter supportsSSL $ map splitHypernets input

supportsSSL :: [String] -> Bool
supportsSSL ip = or $ map (hasBABs babs hyper) [0..(length babs) - 1]
    where babs = map convertToBAB abas
          abas = concat $ map getABAs super
          (super, hyper) = split2 ip
         
convertToBAB :: String -> String
convertToBAB (x:y:z:[]) 
    | x == z = (y:x:y:[])
convertToBAB _ = error "Input must be ABA"
    
hasBABs :: [String] -> [String] -> Int -> Bool    
hasBABs babs hyper i
    | length babs == 0 = False
    | otherwise = (babs !! i) `containedIn` hyper
        
containedIn :: Eq a => [a] -> [[a]] -> Bool
x `containedIn` y = or $ map (isInfixOf x) y

getABAs :: String -> [String]
getABAs (x:y:z:rest)
    | x == z && x /= y = (x:y:z:[]) : getABAs (y:z:rest)
    | otherwise = getABAs (y:z:rest)
getABAs _ = []

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 [x] = ([x], [])
split2 (x:y:xys) = (x:xs, y:ys)
    where (xs, ys) = split2 xys

splitHypernets :: String -> [String]
splitHypernets = splitWhen $ \x -> x == '[' || x == ']'