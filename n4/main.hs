import qualified Common
import Data.List
import Data.Array

(!?) :: (Ix i) => Array i e -> i -> Maybe e
a !? idx = if inRange (bounds a) idx
           then Just (a!idx)
           else Nothing

-- only works if numbers are sorted
getWinners :: ([Int], [Int]) -> [Int]
getWinners ([],[]) = []
getWinners ([], x) = []
getWinners (x, []) = []
getWinners (x:xs, y:ys) | x < y = getWinners (xs, y:ys)
                        | x > y = getWinners (x:xs, ys)
                        | x == y = y : getWinners (x:xs, ys)

splitWinnerDraws :: String -> ([Int], [Int])
splitWinnerDraws [] = ([],[])
splitWinnerDraws x = (sort winners, sort draws)
    where winners = map read winnerStrings
          winnerStrings = Common.splitAtMultiple ' ' part1Stripped
          part1Stripped = Common.stripFirst ' ' (snd $ Common.splitAt ':' part1)
          (part1,part2) = Common.splitAt '|' x
          draws = map read drawStrings
          drawStrings = Common.splitAtMultiple ' ' part2Stripped
          part2Stripped = Common.stripFirst ' ' part2

appendMultiple :: Array Int String -> [String] -> Int -> Int -> [String]
appendMultiple _ stack _ 0 = stack
appendMultiple arr stack start number = case arr !? start of 
                                            Just e -> e : appendMultiple arr stack (start + 1) (number - 1) 
                                            _ -> stack

calcValue :: [Int] -> Int
calcValue [] = 0
calcValue x = 2^(length x-1)

powersOfTwo :: Int -> [Int]
powersOfTwo x = [2^y | y <- [0..x-1]]

solve1 :: [String] -> Int
solve1 = foldr ((+) . calcValue . getWinners . splitWinnerDraws) 0

solveHelper_ :: Array Int String -> Array Int Int -> Int -> Array Int Int
solveHelper_ _ arr idx | idx == snd (bounds arr) = arr
solveHelper_ orig arr idx = solveHelper_ orig newArr (idx+1)
    where numWinners = length ((getWinners . splitWinnerDraws) (orig ! idx))
          multiplier = arr ! idx
          newArr = if numWinners > 0 
                   then arr // [(i, (arr ! i) + multiplier) | i <- [idx+1..idx+numWinners]]
                   else arr

ones :: Int -> Array Int Int
ones 0 = listArray (0,-1) []
ones x = listArray (0, x-1) [1 | y <- [0..x-1]]

solve2_ :: [String] -> Int
solve2_ [] = 0
solve2_ x = sum (solveHelper_ orig (ones (length x)) 0)
    where orig = listArray (0, length x - 1) x

main = do 
        splitLines <- Common.readAndSplit "input.txt"
        print (solve1 splitLines)
        print (solve2_ splitLines)