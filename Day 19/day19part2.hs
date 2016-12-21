import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

main = do  
        let input  = 3018458
            result = compute input
        print result

compute :: Int -> Int
compute input = Seq.index seq 0 
    where seq = until (\e -> Seq.length e == 1) stealGifts (Seq.fromList [1..input])

stealGifts :: Seq Int -> Seq Int
stealGifts elves = right Seq.>< left
    where index = length elves `div` 2
          (left, right) = Seq.splitAt 1 $ Seq.deleteAt index elves