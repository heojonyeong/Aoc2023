import Data.Char
import Data.Array
import qualified Data.Vector
import qualified Common

data Lens = Lens String Int
    deriving(Show)

solve1 :: String -> Integer -> Integer -> Integer
solve1 [] sub total = total+sub
solve1 (x:xs) sub total | x==',' = solve1 xs 0 (total+sub)
                             | x=='\n' || x=='\r' = solve1 xs sub total
                             | otherwise = solve1 xs newSub total
    where newSub = ((sub + fromIntegral (ord x)) * 17) `mod` 256

hash :: String -> Int
hash s = hash_ s 0
    where hash_ [] sub = sub
          hash_ (x:xs) sub = hash_ xs (((sub + fromIntegral (ord x))*17) `mod` 256)

emptyTable :: Array Int (Data.Vector.Vector Lens)
emptyTable = listArray (0,255::Int) [Data.Vector.empty :: Data.Vector.Vector Lens | _ <- [0..255]]

calcFocussingNumber :: Array Int (Data.Vector.Vector Lens) -> Int
calcFocussingNumber x = sum (map calcNumber_ (assocs x))
    where calcNumber_ (bIdx, v) = (bIdx+1) * foldr (\(i,Lens _ power) p -> p+(i+1)*power) 0 (Data.Vector.indexed v)

parseInstruction :: String -> (String, Char, Int)
parseInstruction x | r =='-' = (name, r, 0)
                   | r =='=' = (name, r, read rs)
    where name = takeWhile (\a -> a /= '=' && a /= '-') x
          (r:rs) = drop (length name) x

insertTable :: Array Int (Data.Vector.Vector Lens) -> String -> Array Int (Data.Vector.Vector Lens)
insertTable arr x | inst == '-' = arr // [(hs, Data.Vector.filter (\(Lens n f) -> n/=name) v)]
                  | inst == '=' = case Data.Vector.findIndex (\(Lens n f) -> n==name) v of
                                    Nothing -> arr // [(hs, Data.Vector.snoc v (Lens name f))]
                                    Just i -> arr // [(hs, v Data.Vector.// [(i,Lens name f)])]

    where (name, inst, f) = parseInstruction x 
          hs :: Int = hash name
          v = arr ! hs

solve2 :: String -> Int
solve2 x = calcFocussingNumber arr
    where arr = foldl (\t s -> insertTable t s) emptyTable (Common.splitAtMultiple ',' x)

main :: IO ()
main = do
    file <- getLine
    input <- readFile file
    print $ solve1 input 0 0
    print $ solve2 input