import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import Data.Char
import qualified AStar

main = do  
        contents <- readFile "day11input.txt"
        let result = compute $ lines contents
        print result
        
type Pair = (Int, Int)           -- (Floor of microchip, Floor of generator)
type State = (Int, [Pair])       -- (Floor of elevator, List of Pairs)     

compute :: [String] -> Int
compute input = cost
    where Just (cost, path) = AStar.search initState isGoalState nextStates heuristic
          initState = generateInitialState input
          
isGoalState :: State -> Bool
isGoalState (_, pairs) = and $ map (\(i,j) -> i == 3 && j == 3) pairs

nextStates :: State -> [(State, Int)]
nextStates (elev, pairs) = map (\s -> (sortState s, 1)) validStates
    where validStates = filter validState newStates
          newStates = concat $ map (makeMove (elev, pairs)) ranges
          ranges = filter (\x -> length x < 3) seq
          seq = tail $ subsequences $ elemIndices elev $ unPairList pairs
          
unPairList :: [(a,a)] -> [a]
unPairList [] = []
unPairList ((x, y):xs) = x : y : unPairList xs 

makeMove :: State -> [Int] -> [State]
makeMove (elev, pairs) moves = [moveUp] ++ [moveDown]
    where moveUp = (elev + 1, foldl (move elev True) pairs moves)
          moveDown = (elev - 1, foldl (move elev False) pairs moves)
          
move :: Int -> Bool -> [Pair] -> Int -> [Pair]
move floor up ((m,g):pairs) i
    | i == 0 = (newElem m, g) : pairs
    | i == 1 = (m, newElem g) : pairs
    | otherwise = (m, g) : move floor up pairs (i - 2)
    where newElem n = if up then n + 1 else n - 1

validState :: State -> Bool
validState state@(elev, pairs) = elev > -1 && elev < 4 && unfriedState pairs pairs
    
unfriedState :: [Pair] -> [Pair] -> Bool
unfriedState _ [] = True
unfriedState originalPairs ((m,g):pairs)
    | m == g = rest
    | otherwise = isNothing (find (\(a, b) -> b == m) (delete (m,g) originalPairs)) && rest
    where rest = unfriedState originalPairs pairs

heuristic :: State -> Int
heuristic (_, pairs) = sum $ map (\(i, j) -> 3 - i + 3 - j) pairs

sortState :: State -> State
sortState (elev, pairs) = (elev, sort pairs)

generateInitialState :: [String] -> State
generateInitialState input = (0, sort $ state ++ newElems)
    where newElems = [(0,0),(0,0)]
          state = collate . concat $ map (\i -> generateFloor i $ parsedInput !! i) [0 .. length parsedInput - 1]
          generateFloor i floor = map (\word -> (takeWhile (/= '-') word, "compatible" `isInfixOf` word, i)) floor
          parsedInput = map (concat . tail . splitWhen (== "a") . drop 4 . filter validWord . words) input
          validWord x = and $ map (\w -> not $ isPrefixOf w x) ["and", "generator", "microchip"]

collate :: [(String, Bool, Int)] -> [Pair]
collate [] = [] 
collate ((x, b, i):xs) = pair : collate (delete other xs)
    where pair = if b then (i,i') else (i',i)
          Just other@(_,_,i') = find (\(x',_,_) -> x == x') xs  