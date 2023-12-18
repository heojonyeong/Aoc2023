import qualified Common

solve1 :: [String] -> Int -> Int -> Int -> Int -> Int
solve1 [] _ _ area boundary = boundary + (area - (boundary `div` 2) + 1) --pick formula
solve1 (l:ls) yStart yCur area boundary | d == "D" = solve1 ls yStart (yCur-num) area (boundary+num)
                                        | d == "U" = solve1 ls yStart (yCur+num) area (boundary+num)
                                        | otherwise = solve1 ls yStart yCur (area+sign*nArea) (boundary+num)
    where [d, n, _] = Common.splitAtMultiple ' ' l
          num = (read n)
          nArea = num * (yCur - yStart)
          sign | d == "L" = -1
               | d == "R" =  1

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    print $ solve1 splitLines (-1000) 0 0 0