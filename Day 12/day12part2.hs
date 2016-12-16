import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe

main = do  
        contents <- readFile "day12input.txt"
        let result = compute $ map words $ lines contents
        print result
        
type State = Map.Map Char Int

initialState :: State
initialState = Map.fromList [('a',0),('b',0),('c',1),('d',0)]
       
compute input = fromJust . Map.lookup 'a' $ operate initialState input 0

operate :: State -> [[String]] -> Int -> State
operate state instrs pc 
    | pc > length instrs - 1 = state
    | otherwise = operate state' instrs pc'
    where pc' = pc + offset
          (state', offset) = executeInstr state $ instrs !! pc

executeInstr :: State -> [String] -> (State, Int)
executeInstr state ["cpy", a, b] = (cpy state a b, 1)
executeInstr state ["inc", a] = (inc state a, 1)
executeInstr state ["dec", a] = (dec state a, 1)
executeInstr state ["jnz", a, b] = jnz state a $ read $ b
    
cpy :: State -> String -> String -> State
cpy state val reg = Map.insert (head reg) newVal state
    where regVal = fromJust $ Map.lookup (head val) state
          newVal = if isNumStr val then read val else regVal

isNumStr :: String -> Bool
isNumStr str = and $ map isDigit str
          
inc :: State -> String -> State
inc state reg = Map.adjust (+ 1) (head reg) state

dec :: State -> String -> State
dec state reg = Map.adjust (subtract 1) (head reg) state

jnz :: State -> String -> Int -> (State, Int)
jnz state reg offset
    | isNumStr reg = if read reg /= 0 then (state, offset) else (state, 1)
    | otherwise = if fromJust val /= 0 then (state, offset) else (state, 1)
    where val = Map.lookup (head reg) state