import qualified Common
import Data.Map.Lazy

buildMap :: [String] -> Map String (String, String)
buildMap x = buildMap_ x empty
    where buildMap_ [] y = y
          buildMap_ (y:ys) m = buildMap_ ys (insert key value m)
            where (key_,value_) = Common.splitAt '=' y
                  key = Prelude.take 3 key_
                  value = (Prelude.take 3 (Prelude.drop 2 value_), Prelude.take 3 (Prelude.drop 7 value_)) 

solve1 :: [String] -> Integer
solve1 x = solve1_ sequence m "AAA" 0
    where sequence = cycle $ head x
          m = buildMap (Prelude.drop 2 x)

          solve1_ (s:ss) m cur steps = if cur == "ZZZ"
                                       then steps
                                       else case s of
                                             'L' -> solve1_ ss m (fst(m ! cur)) (steps+1)
                                             'R' -> solve1_ ss m (snd(m ! cur)) (steps+1)

main = do 
        input <- getLine
        splitLines <- Common.readAndSplit input
        print(solve1 splitLines)