import Data.Hash.MD5

main = do  
        let input  = "ojvtpuvg"
            result = compute input
        print result

compute :: String -> String
compute input = take 8 $ findPass input 0

findPass :: String -> Int -> String
findPass input index1 = c : findPass input (index2 + 1)
    where (c, index2) = computeHash input index1
    
computeHash :: String -> Int -> (Char, Int)
computeHash input index
    | take 5 hash == "00000" = (hash !! 5, index)
    | otherwise = computeHash input (index + 1)
    where hash = md5s $ Str $ input ++ show index