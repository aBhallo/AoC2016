import Data.List
import Data.List.Split
import Data.Maybe

main = do  
        let input  = ("10001001100000001", 272)
            result = compute input
        print result
   
type Bits = [Bool]
   
compute :: (String, Int) -> String
compute (input, size) = toString $ check $ take size disk
    where check = until (odd.length) checksum
          disk = fillDisk (toBits input) size

fillDisk :: Bits -> Int -> Bits
fillDisk bits size = fromJust $ find (\x -> length x >= size) $ iterate dragonCurve bits

dragonCurve :: Bits -> Bits
dragonCurve a = a ++ [False] ++ b
    where b = reverse $ map not a
    
checksum :: Bits -> Bits
checksum input = map (\[a,b] -> a == b) $ chunksOf 2 input 
          
smallestEvenFactor :: Int -> Int          
smallestEvenFactor n
    | next `mod` 2 == 0 = smallestEvenFactor next
    | otherwise = next
    where next = n `div` 2
    
toBits :: String -> Bits
toBits = map (== '1')

toString :: Bits -> String
toString = map (\b -> if b then '1' else '0')