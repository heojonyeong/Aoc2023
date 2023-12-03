import qualified Common
import Data.Array

(!?) :: (Ix i) => Array i e -> i -> Maybe e
a !? idx = if inRange (bounds a) idx
           then Just (a!idx)
           else Nothing

neighbors :: Array (Int, Int) Char -> (Int, Int) -> [Char]
neighbors a (x,y) = foldr concat [] [a !? (i,j) | i <- [x-1, x, x+1], j<-[y-1,y,y+1]]
    where concat element list = case element of
                                    Just e -> e:list
                                    _ -> list

windowingHelper :: Array (Int, Int) Char -> (Int, Int) -> Integer -> Integer -> Bool -> Integer
windowingHelper arr idx@(idx1, idx2) cur total foundSymbol | stop = total
                                                           | atEnd && isDigit && takeNum = windowingHelper arr (idx1+1,0) 0 (total + cur*10 + nowAsNum) False
                                                           | atEnd && foundSymbol = windowingHelper arr (idx1+1, 0) 0 (total + cur) False
                                                           | atEnd = windowingHelper arr (idx1+1, 0) 0 total False
                                                           | isDigit = windowingHelper arr (idx1, idx2+1) (cur*10 + nowAsNum) total takeNum 
                                                           | foundSymbol = windowingHelper arr (idx1, idx2+1) 0 (total+cur) False
                                                           | otherwise = windowingHelper arr (idx1, idx2+1) 0 total False
    
    where now = arr ! idx
          takeNum = foundSymbol || any Common.isSymbol (neighbors arr idx)
          isDigit = Common.isDigit now
          rowEndIdx = snd (snd (bounds arr))
          atEnd = idx2 == rowEndIdx
          stop = not (inRange (bounds arr) idx)
          nowAsNum :: Integer = read [now]

solve1 :: Array (Int, Int) Char -> Integer
solve1 x = windowingHelper x (0,0) 0 0 False

main = do 
        splitLines <- Common.readAndSplit "input_sample.txt"
        let arr = Common.to2DArray splitLines
        print(solve1 arr)