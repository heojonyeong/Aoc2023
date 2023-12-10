import qualified Common
import Data.Array
import Data.List (repeat)

data Direction = Left | Right | Up | Down
data Mode = In | Out

findStart :: Array (Int, Int) Char -> ((Int,Int), Direction)
findStart x | unwrap $ x Common.!? (sy, sx-1) >>= (\a -> return (a `elem` "-FL")) = (s, Main.Left)
            | unwrap $ x Common.!? (sy, sx+1) >>= (\a -> return (a `elem` "-7J")) = (s, Main.Right)
            | unwrap $ x Common.!? (sy+1, sx) >>= (\a -> return (a `elem` "|7F")) = (s, Main.Up)
            | unwrap $ x Common.!? (sy-1, sx) >>= (\a -> return (a `elem` "|LJ")) = (s, Main.Down)

    where s@(sy,sx) = head [ i | (i,e) <- assocs x, e=='S' ] 
          unwrap m = case m of { Just b -> b; Nothing -> False }

nextIdx :: Array (Int, Int) Char -> (Int,Int) -> Direction -> ((Int, Int), Direction)
nextIdx a (y,x) d = (nP, nD)
        
        where nP = case d of 
                    Main.Left -> (y,x-1)
                    Main.Right -> (y,x+1)
                    Main.Up -> (y-1,x)
                    Main.Down -> (y+1,x)
              nD = case a!nP of
                        'F' -> case d of { Main.Left -> Main.Down; Main.Up -> Main.Right }
                        'L' -> case d of { Main.Down -> Main.Right; Main.Left -> Main.Up }
                        'J' -> case d of { Main.Down -> Main.Left; Main.Right -> Main.Up }
                        '7' -> case d of { Main.Up -> Main.Left; Main.Right -> Main.Down }
                        _ -> d

path :: Array (Int,Int) Char -> [(Int,Int)]
path a = path_ a s d []

    where (s,d) = findStart a
          path_ a i d p = if a!i == 'S' && p /= []
                            then (i:p)
                            else path_ a newI newD (i:p)
            where (newI, newD) = nextIdx a i d


startSymbol :: Array (Int, Int) Char -> [(Int,Int)] -> Char
startSymbol a idxs | fx == lx = '-'
                   | fy == ly = '|'
                   | fy < ly && fx < lx = if sy == fy then '7' else 'L'
                   | fy < ly && fx > lx = if sy == fy then 'F' else 'J'
                   | fy > ly && fx < lx = if sy == fy then 'J' else 'F'
                   | fy > ly && fx > lx = if sy == fy then 'L' else '7'
                    
    where (fy,fx) = idxs !! 1
          (ly,lx) = idxs !! (length idxs - 2)
          (sy,sx) = last idxs
          

cleanedArray :: Array (Int,Int) Char -> [(Int,Int)] -> Array (Int,Int) Char
cleanedArray a p = ((wipe a) // (zip p (map (\i -> a ! i) p))) // [startPair]
    where wipe arr = array (bounds a) (zip (indices a) (map (const '.') (elems a)))
          startPair = (last p,startSymbol a p)

connected :: Char -> Char -> Bool
connected a b | a == '-' && (b=='-' || b=='J' || b=='7') = True
              | a == 'F' && (b=='-' || b=='J' || b=='7') = True
              | a == 'L' && (b=='-' || b=='J' || b=='7') = True
              | otherwise = False

drawBoundary :: Array (Int, Int) Char -> Array (Int, Int) Char
drawBoundary a = array ((ly-1,lx-1),(hy+1,hx+1)) (newPoints ++ assocs a)
    where newPoints = upper ++ lower ++ left ++ right
          ((ly,lx),(hy,hx)) = bounds a
          upper = [((ly-1,x),'.') | x <- [lx-1..hx+1]]
          lower = [((hy+1,x),'.') | x <- [lx-1..hx+1]]
          left = [((y, lx-1), '.') | y <- [ly..hy]]
          right = [((y, hx+1), '.') | y <- [ly..hy]]

sweep_ :: Array (Int, Int) Char -> (Int,Int) -> Mode -> Int -> Int
sweep_ a (y,x) mode count = if x == hx then count else sweep_ a (y,newX) newMode newCount
    where cur = a!(y,x)
          (_,(_,hx)) = bounds a

          newCount = if cur == '.'
                     then case mode of
                            In  -> count+1
                            Out -> count
                     else count

          findExit a (y,x) = if a!(y,x+1) `elem` "7,J" then ((x+1),a!(y,x+1)) else findExit a (y,x+1)
          (cIdx, closer) = findExit a (y,x)
          switchedMode = case mode of { In -> Out; Out -> In }
          newX = case cur of 
                  'F' -> cIdx+1
                  'L' -> cIdx+1
                  _   -> x+1
          newMode = case cur of 
                     'F' -> if closer == '7' then mode else switchedMode
                     'L' -> if closer == 'J' then mode else switchedMode
                     '.' -> mode
                     _   -> switchedMode

solve1 :: Array (Int,Int) Char -> Int
solve1 a = (length $ path a) `div` 2 -- length can never be odd

solve2 :: Array (Int,Int) Char -> Int
solve2 a = sum (map (\y -> sweep_ a (y,-1) Out 0) [0..maxY])
    where (_,(maxY,_)) = bounds a

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    let arr :: Array (Int,Int) Char = Common.to2DArray splitLines
    print $ solve1 arr
    let p = path arr
    let updated = drawBoundary (cleanedArray arr p)
    print $ solve2 updated