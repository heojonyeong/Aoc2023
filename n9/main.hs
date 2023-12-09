import qualified Common

buildInitial :: ([Integer] -> [Integer]) -> String -> [Integer]
buildInitial transform = transform . (map read . Common.splitAtMultiple ' ')

buildStack :: ([Integer] -> [Integer]) -> String -> [[Integer]]
buildStack transform x = buildStack_ [initial]

    where initial = buildInitial transform x
          buildStack_ :: [[Integer]] -> [[Integer]]
          buildStack_ (y:ys) = if done then (y:ys) else buildStack_ (diffList : (y:ys))
            where done = all (==0) y      
                  diffList = map (uncurry (-)) (zip y (tail y))

solveStack :: ([Integer] -> [Integer]) -> String -> Integer
solveStack transform x = solveStack_ stack
    where stack = buildStack transform x
          
          solveStack_ :: [[Integer]] -> Integer
          solveStack_ y = sum $ map head y

solve1 :: [String] -> Integer
solve1 x = sum $ map (solveStack reverse) x

solve2 :: [String] -> Integer
solve2 x = sum $ map (solveStack id) x

main = do 
        file <- getLine
        splitLines <- Common.readAndSplit file
        print $ solve1 splitLines
        print $ solve2 splitLines
