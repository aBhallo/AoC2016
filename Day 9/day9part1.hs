import Data.List
import Data.List.Split

main = do  
        contents <- readFile "day9input.txt"
        let result = compute contents
        print result
       
type Marker = (Int, Int)
  
compute :: String -> Int
compute input = length $ decompress input

decompress :: String -> String
decompress "" = ""
decompress input = init ++ appliedMarker ++ decompress toApply
    where (appliedMarker, toApply) = applyMarker marker rest
          (init, marker, rest) = splitFirstMarker input

applyMarker :: Maybe Marker -> String -> (String, String)
applyMarker Nothing str = ("", str)
applyMarker (Just (i, n)) str = (take (n * i) $ cycle $ take i str, drop i str)

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