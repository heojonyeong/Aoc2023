import qualified Common
import Data.Array

data Direction = Left | Right | Up | Down

-- don't check if any if out of bounds
findStart :: Array (Int, Int) Char -> ((Int,Int), Direction)
findStart x | x!(sy, sx-1) `elem` "-FL" = (s, Main.Left)
            | x!(sy, sx+1) `elem` "-7J" = (s, Main.Right)
            | x!(sy+1, sx) `elem` "|7F" = (s, Main.Up)
            | x!(sy-1, sx) `elem` "|LJ" = (s, Main.Down)

    where s@(sy,sx) = head [ i | (i,e) <- assocs x, e=='S' ] 

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

solve1 :: Array (Int,Int) Char -> Int
solve1 a = solve1_ a s d 0
    
    where (s,d) = findStart a
          solve1_ a i d l = if a!i == 'S' && l>0
                            then l `div` 2 -- l cannot be odd
                            else solve1_ a newI newD (l+1)
            where (newI, newD) = nextIdx a i d

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    let arr :: Array (Int,Int) Char = Common.to2DArray splitLines
    print $ solve1 arr