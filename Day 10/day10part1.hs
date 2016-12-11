import Data.List
import Data.Char
import Data.Maybe
import Data.Either
import Data.List.Utils 

main = do  
        contents <- readFile "day10input.txt"
        let result = compute $ lines contents
        print result
       
type Value = Int
type BotID = Int
type Bot = (BotID, [Value])
type GiveInstruction = (BotID, BotID, BotID)
type TakeInstruction = (BotID, Value)
type State = [Bot]

initState :: State
initState = []

compute :: [String] -> BotID
compute input = shareAndCheck shareInstrs initialReceive
    where (shareInstrs, receiveInstrs) = parseInput input
          initialReceive = foldl receive initState receiveInstrs
   
shareAndCheck :: [GiveInstruction] -> State -> BotID
shareAndCheck instructions state  
    | isJust target = fromJust target
    | otherwise = shareAndCheck instructions $ share state instructions
    where target = getTarget state
   
getTarget :: State -> Maybe BotID
getTarget [] = Nothing
getTarget ((b, [x, y]) : bs)
    | (x == 17 && y == 61) || (x == 61 && y == 17) = Just b
    | otherwise = getTarget bs
getTarget (_ : bs) = getTarget bs   
   
share :: State -> [GiveInstruction] -> State
share state instructions = receive (receive (empty state b) (r1, low)) (r2, high)
    where (b, l) = fromJust $ find (\(x, l) -> length l == 2) state
          (_, r1, r2) = fromJust $ find (\(s,_,_) -> s == b) instructions
          [low, high] = sort l
          
empty :: State -> BotID -> State
empty ((b, l) : bs) b'
    | b == b' = (b, []) : bs
    | otherwise = (b, l) : empty bs b'

receive :: State -> TakeInstruction -> State
receive [] (id, val) = [(id, [val])]
receive ((b, vals) : bs) (id, val) 
    | b == id = (b, val : vals) : bs
    | otherwise = (b, vals) : receive bs (id, val)

--- Input parsing ---
    
parseInput :: [String] -> ([GiveInstruction],[TakeInstruction])
parseInput input = (lefts parsedInput, rights parsedInput)
    where parsedInput =  map parseInstr input

parseInstr :: String -> Either GiveInstruction TakeInstruction
parseInstr instr = case numbers of
    (x:y:z:[]) -> Left (read x, read y, read z)
    (x:y:[]) -> Right (read y, read x)
    where numbers = filter isNumStr $ words $ replace "output " "bot 100" instr -- Replace output with "bot 100..." to differenciate
     
isNumStr:: String -> Bool
isNumStr str = and $ map (\c -> isDigit c || c == '-') str