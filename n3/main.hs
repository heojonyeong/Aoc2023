import qualified Common
import Data.Array
import Data.List

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

findLeftMostDigitIdx :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int)
findLeftMostDigitIdx arr (i,j) = case arr !? (i, j-1) of
                                    Just e -> if Common.isDigit e
                                              then findLeftMostDigitIdx arr (i, j-1)
                                              else (i, j)
                                    _ -> (i,j) 

-- find all Numbers around a certain index
findNumbersAround :: Array (Int, Int) Char -> (Int, Int) -> [Integer]
findNumbersAround arr (idx1,idx2) = filter (/=0) numbers 
    where startIdx = [(i,j) | i <- [idx1-1, idx1, idx1+1], j <- [idx2 -1, idx2, idx2+1]]
          filtered :: [(Int,Int)] -> [(Int,Int)]
          filtered [] = []
          filtered (x:xs) = case arr !? x of
                              Just e -> if Common.isDigit e
                                        then x : filtered xs
                                        else filtered xs
                              _ -> filtered xs
          leftMostDigits = map head (group $ sort $  map (findLeftMostDigitIdx arr) (filtered startIdx))
          numbers = map (numAt arr) leftMostDigits


numAt :: Array (Int, Int) Char -> (Int, Int) -> Integer
numAt arr idx@(idx1, idx2) = helper 0 (idx1, idx2)
    where helper :: Integer -> (Int, Int) -> Integer
          helper total (idx1,idx2) = case arr !? (idx1,idx2) of
                                        Just e -> if Common.isDigit e
                                                   then helper (total*10 + read [e]) (idx1,idx2+1)
                                                   else total
                                        _ -> total

solve1 :: Array (Int, Int) Char -> Integer
solve1 x = windowingHelper x (0,0) 0 0 False

solve2Helper :: Array (Int, Int) Char -> (Int, Int) -> Integer -> Integer
solve2Helper arr idx@(idx1,idx2) total | stop = total
                                       | isStar = case numsAround of
                                                   [a,b] -> solve2Helper arr (next idx) total + a*b
                                                   _ -> solve2Helper arr (next idx) total
                                       | otherwise = solve2Helper arr (next idx) total
    where now = arr ! idx
          isStar = now == '*'
          numsAround = findNumbersAround arr idx
          rowEndIdx = snd (snd (bounds arr))
          atEnd = idx2 == rowEndIdx
          stop = not (inRange (bounds arr) idx)
          next (x,y) = if atEnd
                       then (x+1, 0)
                       else (x, y+1)

solve2:: Array (Int, Int) Char -> Integer
solve2 arr = solve2Helper arr (fst (bounds arr)) 0

main = do 
        splitLines <- Common.readAndSplit "input.txt"
        let arr = Common.to2DArray splitLines
        print(solve1 arr)
        print(solve2 arr)