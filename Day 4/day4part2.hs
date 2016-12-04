import System.IO  
import Data.List
import Data.Char
import Data.Maybe

main = do  
        handle <- openFile "day4input.txt" ReadMode
        contents <- hGetContents handle
        let splitContent = words contents
            result = compute splitContent
        print result
        hClose handle   
   
type Freq = (Char, Int)
type Table = [Freq]   
type Room = (String, Int, String)

compute :: [String] -> Int
compute input = fromJust $ lookup "northpole object storage" $ map decrypt validRooms
    where validRooms = filter isValidRoom parsedInput
          parsedInput = map splitData input
      
decrypt :: Room -> (String, Int)
decrypt (enc, id, _) = (map (decryptChar id) enc, id)

decryptChar :: Int -> Char -> Char
decryptChar n c 
    | c == '-' = ' '
    | otherwise = chr (if i > 122 then i - 26 else i)
    where i = ord c + (n `mod` 26)
  
isValidRoom :: Room -> Bool  
isValidRoom (enc, id, sumCheck) = checkSum sumCheck (take 5 sortedTable)
    where sortedTable = sortBy order table
          table = tabulate filteredEnc []
          filteredEnc = sort $ filter (/= '-') enc

checkSum :: String -> Table -> Bool
checkSum [] [] = True
checkSum (x:xs) ((y,_):ys)
    | x == y = checkSum xs ys
checkSum _ _ = False
          
tabulate :: String -> Table -> Table
tabulate [] table = table
tabulate (x:xs) table = tabulate xs (tally x table)

tally :: Char -> Table -> Table
tally c [] = [(c,1)]
tally c ((a,n):table)
    | c == a = (a,(n + 1)):table
    | otherwise = (a,n):tally c table

order :: Freq -> Freq -> Ordering
order (a, b) (c, d) = compare d b 

splitData :: String -> Room
splitData input = (encrypted, sectorID, sumCheck)
    where encrypted = init split1
          sectorID = read split3
          sumCheck = tail.init $ split4
          (split3, split4) = span (/= '[') split2
          (split1, split2) = span (not.isDigit) input