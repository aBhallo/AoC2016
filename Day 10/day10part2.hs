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

compute :: [String] -> Int
compute input = multiply $ shareAndCheck shareInstrs initialReceive
    where (shareInstrs, receiveInstrs) = parseInput input
          initialReceive = foldl receive initState receiveInstrs

multiply :: State -> Int
multiply [] = 1
multiply ((b, [y]):bs)
    | isOutput b = y * multiply bs
multiply (_:bs) = multiply bs

outputs :: [BotID]
outputs = [1000,1001,1002]

isOutput :: BotID -> Bool
isOutput b = b `elem` outputs

shareAndCheck :: [GiveInstruction] -> State -> State
shareAndCheck instructions state  
    | isTarget state $ length outputs = state
    | otherwise = shareAndCheck instructions $ share state instructions
   
isTarget :: State -> Int -> Bool
isTarget _ 0 = True
isTarget [] _ = False
isTarget ((b, [x]) : bs) n
    | isOutput b = isTarget bs (n-1)
    | otherwise = isTarget bs n 
isTarget (_ : bs) n = isTarget bs n   

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