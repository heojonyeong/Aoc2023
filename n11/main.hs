import qualified Common
import qualified Data.List

findGalaxiesLine :: String -> [Int]
findGalaxiesLine x = findGalaxiesLine_ x 0 []
    where findGalaxiesLine_ [] _ list = list
          findGalaxiesLine_ (x:xs) idx list = if x=='#'
                                              then findGalaxiesLine_ xs (idx+1) (idx:list)
                                              else findGalaxiesLine_ xs (idx+1) list

findGalaxies :: [String] -> [(Int,Int)]
findGalaxies x = findGalaxies_ x 0
    where findGalaxies_ [] _ = []
          findGalaxies_ (x:xs) idx = zip (repeat idx) (findGalaxiesLine x) ++ findGalaxies_ xs (idx+1)

expandX :: [(Int,Int)] -> [(Int,Int)]
expandX x = expandX_ sorted 0 (0,0)
    where sorted = Data.List.sortBy comp x
          comp (_,x1) (_,x2) = compare x1 x2

          expandX_ :: [(Int,Int)] -> Int -> (Int,Int) -> [(Int,Int)]
          expandX_ [] _ _ = []
          expandX_ ((y,x):xs) adder (_,lx) = if diff > 0
                                             then (y,x+adder+diff): expandX_ xs (adder+diff) (y,x)
                                             else (y,x+adder): expandX_ xs adder (y,x)  
            where diff = x - lx - 1

expandY :: [(Int,Int)] -> [(Int,Int)]
expandY x = expandY_ sorted 0 (0,0)
    where sorted = Data.List.sortBy comp x
          comp (y1,_) (y2,_) = compare y1 y2

          expandY_ [] _ _ = []
          expandY_ ((y,x):xs) adder (ly,_) = if diff > 0
                                             then (y+diff+adder,x): expandY_ xs (adder+diff) (y,x)  
                                             else (y+adder, x): expandY_ xs adder (y,x)
            where diff = y - ly - 1

distance :: ((Int,Int), (Int,Int)) -> Int
distance ((ya,xa), (yb,xb)) = abs (ya-yb) + abs (xa-xb)

unorderedPairs :: [(Int,Int)] -> [((Int,Int), (Int,Int))]
unorderedPairs [] = []
unorderedPairs (x:xs) = zip (repeat x) xs ++ unorderedPairs xs

solve1 :: [String] -> Int
solve1 x = sum $ map distance (unorderedPairs $ expandY $ expandX $ findGalaxies x)

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    print $ solve1 splitLines