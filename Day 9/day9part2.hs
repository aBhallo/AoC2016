import Data.List
import Data.List.Split
import Data.Maybe

main = do  
        contents <- readFile "day9input.txt"
        let result = compute contents
        print result
       
type Marker = (Int, Int)
  
compute input = decompressLen input

decompressLen :: String -> Int
decompressLen "" = 0
decompressLen input = length init + (decompressLen (take i rest) * n) + decompressLen (drop i rest) 
    where (i,n) = if isJust marker then fromJust marker else (0,0)
          (init, marker, rest) = splitFirstMarker input
    
splitFirstMarker :: String -> (String, Maybe Marker, String)
splitFirstMarker str = (init, marker, unchanged)
    where (marker, unchanged) = splitMarker index rest'
          index = findIndex (== ')') rest'
          rest' = drop 1 rest
          (init, rest) = span (/= '(') str
          
splitMarker :: Maybe Int -> String -> (Maybe Marker, String)
splitMarker Nothing str = (Nothing, str)
splitMarker (Just i) str = (Just $ parseMarker $ take i str, drop (i + 1) str) 
          
parseMarker :: String -> Marker
parseMarker marker = (read i, read n)
    where [i, n] = splitOn "x" marker