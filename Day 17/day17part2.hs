import Data.ByteString.Char8 (pack)
import Crypto.Hash 

main = do  
        let input  = "vkjiggvb"
            result = compute input
        print result

type Coord = (Int, Int)        
type Dir = Char
type State = (Coord, [Dir], String)   
   
bounds :: Int
bounds = 3

start :: Coord
start = (0,0)

initState :: String -> State
initState input = (start, [], input)
        
compute :: String -> Int        
compute input = length $ search [initState input] []

isGoal :: State -> Bool
isGoal (coord, _, _) = coord == (bounds, bounds)
 
nextStates :: State -> [State]
nextStates s@(coord, dirs, input) = up ++ down ++ left ++ right
    where h = take 4 $ getHash (input ++ dirs)
          up = if open (h !! 0) then moveState 'U' s else []
          down = if open (h !! 1) then moveState 'D' s else []
          left = if open (h !! 2) then moveState 'L' s else []
          right = if open (h !! 3) then moveState 'R' s else []

open :: Char -> Bool
open c = c `elem` "bcdef"
 
moveState :: Char -> State -> [State]
moveState 'U' ((x, y), dirs, input)
    | y == 0 = []
    | otherwise = [((x, y - 1), dirs ++ ['U'], input)]
moveState 'D' ((x, y), dirs, input)
    | y == bounds = []
    | otherwise = [((x, y + 1), dirs ++ ['D'], input)]
moveState 'L' ((x, y), dirs, input)
    | x == 0 = []
    | otherwise = [((x - 1, y), dirs ++ ['L'], input)]
moveState 'R' ((x, y), dirs, input)
    | x == bounds = []
    | otherwise = [((x + 1, y), dirs ++ ['R'], input)]

-- Breadth-first search
search :: [State] -> [Dir] -> [Dir]
search [] dirs = dirs
search (current : rest) dirs
    | isGoal current = let (_, path, _) = current in search rest path
    | otherwise = search (rest ++ (nextStates current)) dirs
          
getHash :: String -> String
getHash input = show (hash $ pack input :: Digest MD5)