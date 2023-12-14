import Data.Array
import qualified Common
import qualified Common

sumGauss :: Int -> Int -> Int
sumGauss 0 _ = 0
sumGauss _ 0 = 0
sumGauss start number = (start*(start+1) `div` 2) -  (diff*(diff+1) `div` 2)
    where diff = start - number

countFromNorth :: Array (Int, Int) Char -> (Int,Int) -> Int -> Int -> Int
countFromNorth arr (y,x) yStart count | end = sumGauss yStart count
                                      | cur == '#' = sumGauss yStart count + countFromNorth arr (y+1,x) (hy - y) 0
                                      | cur == 'O' = countFromNorth arr (y+1,x) yStart (count+1) 
                                      | otherwise = countFromNorth arr (y+1,x) yStart count

    where (_,(hy,_)) = bounds arr
          end = y > hy
          cur = arr ! (y,x)

data Mode = North | West | South | East

cycleNorth :: Array (Int, Int) Char -> (Int,Int) -> Int -> Array (Int, Int) Char
cycleNorth arr (y,x) top | end = arr
                         | columnEnd = cycleNorth arr (0, x+1) 0
                         | cur == '#' = cycleNorth arr (y+1,x) (y+1)
                         | cur == 'O' && changed = cycleNorth (arr // [((top,x),'O'),((y,x), '.')]) (y+1,x) (top+1)
                         | cur == 'O' = cycleNorth arr (y+1,x) (top+1)
                         | otherwise = cycleNorth arr (y+1,x) top
    where (_,(hy,hx)) = bounds arr
          columnEnd = y > hy
          end = x > hx
          cur = arr ! (y,x)
          changed = (top,x) /= (y,x)

cycleWest :: Array (Int, Int) Char -> (Int,Int) -> Int -> Array (Int, Int) Char
cycleWest arr (y,x) left  | end = arr
                          | rowEnd = cycleWest arr (y+1, 0) 0
                          | cur == '#' = cycleWest arr (y,x+1) (x+1)
                          | cur == 'O' && changed = cycleWest (arr // [((y,left),'O'),((y,x), '.')]) (y,x+1) (left+1)
                          | cur == 'O' = cycleWest arr (y,x+1) (left+1)
                          | otherwise = cycleWest arr (y,x+1) left
    where (_,(hy,hx)) = bounds arr
          rowEnd = x > hx
          end = y > hy
          cur = arr ! (y,x)
          changed = (y,left) /= (y,x)

cycleSouth :: Array (Int, Int) Char -> (Int,Int) -> Int -> Array (Int, Int) Char
cycleSouth arr (y,x) bottom | end = arr
                            | columnEnd = cycleSouth arr (hy, x+1) hy
                            | cur == '#' = cycleSouth arr (y-1,x) (y-1)
                            | cur == 'O' && changed = cycleSouth (arr // [((bottom,x),'O'),((y,x), '.')]) (y-1,x) (bottom-1)
                            | cur == 'O' = cycleSouth arr (y-1,x) (bottom-1)
                            | otherwise = cycleSouth arr (y-1,x) bottom
    where ((ly,lx),(hy,hx)) = bounds arr
          columnEnd = y < ly
          end = x > hx
          cur = arr ! (y,x)
          changed = (bottom,x) /= (y,x)

cycleEast :: Array (Int, Int) Char -> (Int,Int) -> Int -> Array (Int, Int) Char
cycleEast arr (y,x) right  | end = arr
                           | rowEnd = cycleEast arr (y+1, hx) hx
                           | cur == '#' = cycleEast arr (y,x-1) (x-1)
                           | cur == 'O' && changed = cycleEast (arr // [((y,right),'O'),((y,x), '.')]) (y,x-1) (right-1)
                           | cur == 'O' = cycleEast arr (y,x-1) (right-1)
                           | otherwise = cycleEast arr (y,x-1) right
    where ((ly,lx),(hy,hx)) = bounds arr
          rowEnd = x < lx
          end = y > hy
          cur = arr ! (y,x)
          changed = (y,right) /= (y,x)

cycle :: Array (Int, Int) Char -> Array (Int, Int) Char
cycle arr = cycled_ 
    where (_,(hy,hx)) = bounds arr
          cycled_ = cycleEast (cycleSouth (cycleWest (cycleNorth arr (0,0) 0) (0,0) 0) (hy,0) hy) (0,hx) hx

cycleUntilRepeat :: Array (Int, Int) Char -> Int -> Int
cycleUntilRepeat arr count | arr == cycled = count+1 
                           | count == 100000 = -1
                           | otherwise = cycleUntilRepeat cycled (count+1)
      where cycled = Main.cycle arr

solve1 x = sum [countFromNorth arr (0, x) (hy+1) 0 | x <- [lx..hx]]
    where arr = Common.to2DArray x
          ((_,lx),(hy,hx)) = bounds arr

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    print $ solve1 splitLines
    print $ Common.arrToString (Common.to2DArray splitLines)
    print $ cycleUntilRepeat (Common.to2DArray splitLines) 0 
    --print $ Main.cycle (Common.to2DArray splitLines)
    --print $ Main.cycle (Main.cycle (Common.to2DArray splitLines))
    --print $ Main.cycle (Main.cycle (Main.cycle (Common.to2DArray splitLines)))
    --print $ Main.cycle (Main.cycle (Main.cycle (Main.cycle (Common.to2DArray splitLines))))