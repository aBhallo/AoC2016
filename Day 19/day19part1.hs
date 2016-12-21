main = do  
        let input  = 3018458
            result = compute input
        print result

compute :: Int -> Int
compute input = stealGifts [1..input] []

stealGifts :: [Int] -> [Int] -> Int
stealGifts [x] [] = x
stealGifts (x:_:xs) ys = stealGifts xs (x:ys)
stealGifts xs ys = stealGifts (xs ++ reverse ys) []