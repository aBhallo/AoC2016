main = do  
        contents <- readFile "day18input.txt"
        let result = compute contents
        print result
 
type Row = String
 
compute :: Row -> Int
compute input = sum $ map (length . filter (== '.')) room
    where room = take 40 $ iterate (getNextRow end) input 
          end = length input - 1

getNextRow :: Int -> Row -> Row
getNextRow end prev = map (getTile prev end) [0..end]

getTile :: Row -> Int -> Int -> Char
getTile prev end i = if safe then '.' else '^'
    where left = if i == 0 then '.' else prev !! (i - 1)
          center = prev !! i
          right = if i == end then '.' else prev !! (i + 1)
          safe = isSafe (left, center, right)
          
isSafe :: (Char, Char, Char) -> Bool
isSafe ('^', '^', '.') = False
isSafe ('.', '^', '^') = False
isSafe ('^', '.', '.') = False
isSafe ('.', '.', '^') = False
isSafe _ = True