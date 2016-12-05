import Data.Hash.MD5
import Data.Char
import Data.Maybe

main = do  
        let input = "ojvtpuvg"
            result = compute input
        print result

compute :: String -> String
compute input = collate 0 $ findPass input 0

collate :: Int -> [(Int, Char)] -> String
collate 8 _ = ""
collate n cs = fromJust (lookup n cs) : collate (n + 1) cs

findPass :: String -> Int -> [(Int, Char)]
findPass input index1
    | validPos = (pos, c) : findPass input (index2 + 1)
    | otherwise = findPass input (index2 + 1)
    where ((pos', c), index2) = computeHash input index1
          validPos = isNumber pos' && pos < 8
          pos = digitToInt pos'
          
computeHash :: String -> Int -> ((Char, Char), Int)
computeHash input index
    | take 5 hash == "00000" = ((hash !! 5, hash !! 6), index)
    | otherwise = computeHash input (index + 1)
    where hash = md5s $ Str $ input ++ show index