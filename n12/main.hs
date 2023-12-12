import Data.Array
import Data.Foldable
import qualified Common

splitUp :: String -> (Array Int Char, [Int])
splitUp x = (listArray (0, length left - 1) left, map read (Common.splitAtMultiple ',' right))
    where (left, right) = Common.splitAt ' ' x

countStuff :: Array Int Char -> [Int] -> ([Int],Int)
countStuff arr nums = (qs, sum nums - hashs)
    where qs = filter predicate (indices arr)
          hashs = Common.count '#' arr
          predicate i = arr ! i == '?'

constructChoices :: Int -> Int -> [Int] -> [[Int]]
constructChoices n 0 _ = [[]] 
constructChoices n m [] = []
constructChoices n m (x:xs) = [(x:y) | y <- constructChoices (n-1) (m-1) xs] ++ constructChoices (n-1) m xs

findSequence :: String -> Int -> [Int]
findSequence [] _ = []
findSequence "#" count = [count+1]
findSequence "." 0 = []
findSequence "." count = [count]
findSequence (x:xs) count | x=='#' = findSequence xs (count+1)
                          | count == 0 = findSequence xs count
                          | otherwise = count:findSequence xs 0

allowed :: Array Int Char -> [Int] -> [Int] -> Int
allowed arr seqs spots = if s_ == seqs then 1 else 0
    where replaced = toList (arr // [(s, '#') | s <- spots])
          s_ = findSequence replaced 0

solve1 :: [String] -> Int
solve1 x = sum $ map solveLine x
    where solveLine l = sum $ map (allowed arr seqs) choices
            where (arr, seqs) = splitUp l
                  (qs, maxPlacements) = countStuff arr seqs
                  choices = (uncurry constructChoices) (length qs, maxPlacements) qs

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    print $ solve1 splitLines