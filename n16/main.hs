import qualified Common
import Data.Array

data Dir = U | D | L | R
    deriving (Show, Eq)

data Ray = Ray (Int,Int) Dir
    deriving (Show)

emptyRecord :: ((Int, Int), (Int, Int)) -> Array (Int,Int) [Dir]
emptyRecord b@((ly,lx),(hy,hx)) = array b [((y,x),[]) | x <- [lx..hx], y <- [ly..hy]]

checkRecordAndMark :: Array (Int, Int) [Dir] -> Ray -> (Bool, Array (Int, Int) [Dir])
checkRecordAndMark rec (Ray (y,x) d) | y < ly || y > hy || x <lx || x > hx = (True, rec)
                                     | d `elem` (rec ! (y,x)) = (True, rec)
                                     | otherwise = (False, rec // [((y,x), d:(rec!(y,x)))])
    where ((ly,lx), (hy,hx)) = bounds rec

-- Follow a ray completely until it loops or dies. Returns an updated record and all newly spawned rays
followRay :: Array (Int, Int) Char -> Array (Int,Int) [Dir] -> Ray -> [Ray] -> (Array (Int, Int) [Dir], [Ray])
followRay arr rec r@(Ray (y,x) d) rs = if stop
                                       then (rec, rs)
                                       else followRay arr nRec r_ (rs++rs_)

        where (stop, nRec) = checkRecordAndMark rec r
              (r_,rs_) = next arr r

next :: Array (Int, Int) Char -> Ray -> (Ray, [Ray])
next arr (Ray (y,x) d) = (r,rs)

    where cur = arr ! (y,x)
          (r:rs) | cur == '|' && (d==L || d==R) = [Ray (y-1,x) U, Ray (y+1,x) D]
                 | cur == '-' && (d==U || d==D) = [Ray (y,x-1) L, Ray (y,x+1) R]
                 | cur == '/' && d==U = [Ray (y,x+1) R]
                 | cur == '/' && d==D = [Ray (y,x-1) L]
                 | cur == '/' && d==L = [Ray (y+1,x) D]
                 | cur == '/' && d==R = [Ray (y-1,x) U]
                 | cur == '\\' && d==U = [Ray (y,x-1) L]
                 | cur == '\\' && d==D = [Ray (y,x+1) R]
                 | cur == '\\' && d==L = [Ray (y-1,x) U]
                 | cur == '\\' && d==R = [Ray (y+1,x) D]
                 | d==L = [Ray (y,x-1) L]
                 | d==R = [Ray (y,x+1) R]
                 | d==U = [Ray (y-1,x) U]
                 | d==D = [Ray (y+1,x) D]

followRays :: Array (Int, Int) Char -> Ray -> Array (Int, Int) [Dir]
followRays arr r = followRays_ arr [r] (emptyRecord (bounds arr))

    where followRays_ :: Array (Int, Int) Char -> [Ray] -> Array (Int, Int) [Dir] -> Array (Int, Int) [Dir]
          followRays_ _ [] rec = rec
          followRays_ arr (r:rs) rec = followRays_ arr (rs++nRs) nRec 
            where (nRec, nRs) = followRay arr rec r []

energyFromRay :: Array (Int, Int) Char -> Ray -> Int
energyFromRay arr r = foldr (\a b -> if null a then b else b+1) 0 (followRays arr r)

solve1 :: Array (Int, Int) Char -> Int
solve1 x = energyFromRay x (Ray (0,0) R) 

solve2 :: Array (Int, Int) Char -> Int
solve2 x = maximum $ map (energyFromRay x)  starts 
    where ((ly,lx),(hy,hx)) = bounds x
          starts = [Ray (ly,x) D | x <- [lx..hx]] ++ [Ray (hy,x) U | x <- [lx..hx]] ++[Ray (y,hx) L | y <- [ly..hy]] ++[Ray (y,lx) R | y <- [ly..hy]]

main :: IO ()
main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    let arr = Common.to2DArray splitLines
    print $ solve1 arr
    print $ solve2 arr