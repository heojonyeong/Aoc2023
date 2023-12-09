import qualified Common

buildInitial :: String -> [Integer]
buildInitial = reverse . (map read . Common.splitAtMultiple ' ')

buildStack :: String -> [[Integer]]
buildStack x = buildStack_ [initial]

    where initial = buildInitial x
          buildStack_ :: [[Integer]] -> [[Integer]]
          buildStack_ (y:ys) = if done then (y:ys) else buildStack_ (diffList : (y:ys))
            where done = all (==0) y      
                  diffList = map (\a -> fst a - snd a) (zip y (tail y))

solveStack :: String -> Integer
solveStack x = solveStack_ stack
    where stack = buildStack x
          
          solveStack_ :: [[Integer]] -> Integer
          solveStack_ y = sum $ map head y

solve1 :: [String] -> Integer
solve1 x = sum $ map solveStack x

main = do 
        file <- getLine
        splitLines <- Common.readAndSplit file
        print $ solve1 splitLines
