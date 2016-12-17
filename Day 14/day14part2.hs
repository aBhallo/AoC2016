import Data.Maybe
import Data.List
import Data.ByteString.Char8 (pack)
import Crypto.Hash 

main = do  
        let input  = "ahsbgdzn"
            result = compute input
        print result

type Seq = [String]        
        
compute :: String -> Int
compute input = keys !! 63
    where keys = filter (isKey seq) [1..]
          seq = md5seq input

md5seq :: String -> Seq
md5seq input = map (\i -> hashNTimes 2017 $ input ++ show i) [0..]

hashNTimes :: Int -> String -> String
hashNTimes n input = iterate getHash input !! n

getHash :: String -> String
getHash input = show (hash $ pack input :: Digest MD5)

isKey :: Seq -> Int -> Bool
isKey seq i = hasTriple && check
    where char = fromJust maybeChar
          check = or $ map (hasQuint char) $ take 1000 $ drop (i + 1) seq
          hasTriple = isJust maybeChar
          maybeChar = tripleIndex $ seq !! i

hasQuint :: Char -> String -> Bool          
hasQuint _ [] = False
hasQuint c (x:xs)
    | x == c && take 4 xs == replicate 4 x = True
    | otherwise = hasQuint c xs 
  
tripleIndex :: String -> Maybe Char  
tripleIndex [] = Nothing  
tripleIndex (x:xs)
    | take 2 xs == replicate 2 x = Just x
    | otherwise = tripleIndex xs