import Data.Char

solve1 :: String -> Integer -> Integer -> Integer
solve1 [] sub total = total+sub
solve1 (x:xs) sub total | x==',' = solve1 xs 0 (total+sub)
                             | x=='\n' || x=='\r' = solve1 xs sub total
                             | otherwise = solve1 xs newSub total
    where newSub = ((sub + fromIntegral (ord x)) * 17) `mod` 256

main = do
    file <- getLine
    input <- readFile file
    print $ solve1 input 0 0
