import qualified Common
import Data.List

filterStrings :: String -> [String] -> [String]
filterStrings e [] = []
filterStrings e (x:xs) = if e == x
                         then filterStrings e xs
                         else x : filterStrings e xs

seeds :: String -> [Integer]
seeds x = map read (filterStrings "" (Common.splitAtRepeating ' ' (snd $ Common.splitAt ':' x)))

groupPairs :: [a] -> [(a,a)]
groupPairs [] = []
groupPairs (x:y:xs) = (x,y) : groupPairs xs

seedRanges :: String -> [(Integer, Integer)]
seedRanges x = map (\(a,b) -> (a,a+b-1)) (groupPairs (seeds x))

toMap :: [String] -> [(Integer, Integer, Integer)]
toMap [] = []
toMap [""] = []
toMap (x:xs) = if x /= ""
               then interval : toMap xs
               else []
    where interval = case nums of
                        [a,b,c] -> (a,b,c)
          nums = map read (Common.splitAtRepeating ' ' x)

mapOnce :: Integer -> (Integer, Integer, Integer) -> Integer
mapOnce a (dst,src,length) = if a >= src && a <= (src + length-1)
                             then dst + (a - src)
                             else a

mapInterval :: (Integer, Integer) -> (Integer, Integer, Integer) -> ([(Integer, Integer)], [(Integer, Integer)])
mapInterval (lo,hi) m@(dst, src, l) | lo < src && hi >= src && hi <= end = ([i1], [mapInterval__ i1_])
                                    | lo >= src && lo <=end && hi > end = ([i2_], [mapInterval__ i2])
                                    | lo < src && hi > end = ([i3, i3__], [mapInterval__ i3_])
                                    | lo >= src && hi <= end = ([], [mapInterval__ i4])
                                    | otherwise = ([i4],[])

  where end = src + l - 1
        [i1, i1_] = [(lo, src-1), (src, hi)]
        [i2, i2_] = [(lo, end), (end+1, hi)]
        [i3, i3_, i3__] = [(lo, src-1), (src,end), (end+1, hi)]
        [i4] = [(lo,hi)]

        mapInterval__ :: (Integer, Integer) -> (Integer, Integer)
        mapInterval__ (x,y) = (mapOnce x m, mapOnce y m)

concatIntervals :: [([(Integer, Integer)], [(Integer, Integer)])] -> ([(Integer, Integer)], [(Integer, Integer)])
concatIntervals [] = ([],[])
concatIntervals ((a,b):cs) = (a ++ (fst $ concatIntervals cs), b ++ (snd $ concatIntervals cs))

multiMapInterval_ :: ([(Integer, Integer)], [(Integer, Integer)]) -> [(Integer, Integer, Integer)] -> ([(Integer, Integer)], [(Integer, Integer)])
multiMapInterval_ a [] = a
multiMapInterval_ (unmapped, mapped) (m:ms) = multiMapInterval_ (newUnmapped,mapped ++ newMapped) ms
  where (newUnmapped, newMapped) = concatIntervals $ map (\i -> mapInterval i m) unmapped

multiMapInterval :: (Integer, Integer) -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
multiMapInterval x ms = unmapped ++ mapped
  where (unmapped, mapped) = multiMapInterval_ ([x], []) ms

multiMapIntervals :: [(Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
multiMapIntervals a [] = a
multiMapIntervals [] a = []
multiMapIntervals (x:xs) m = multiMapInterval x m ++ (multiMapIntervals xs m)

totalMap :: [(Integer, Integer)] -> [[(Integer, Integer, Integer)]] -> [(Integer, Integer)]
totalMap [] a = []
totalMap a [] = a
totalMap x (m:ms) = totalMap (multiMapIntervals x m) ms

mapped :: Integer -> [(Integer, Integer, Integer)] -> Integer
mapped a [] = a
mapped a ((dest, src, length):xs) = if a >= src && a <= (src+length-1)
                                    then dest + (a - src)
                                    else mapped a xs

parseMaps :: [String] -> [[(Integer, Integer, Integer)]]
parseMaps [] = []
parseMaps (x:xs) = if "map" `isInfixOf` x
                   then toMap xs : parseMaps xs
                   else parseMaps xs

location :: [[(Integer, Integer, Integer)]] -> Integer -> Integer
location xs a = foldl mapped a xs

solve1 :: [String] -> Integer
solve1 x = minimum locations
    where mappings = parseMaps x
          seedNums = seeds (head x)
          locations = map (location mappings) seedNums

solve2 :: [String] -> Integer
solve2 x = minimum starts
  where seedRanges_ = seedRanges (head x)
        mappings = parseMaps x
        locations = totalMap seedRanges_ mappings
        starts = map fst locations
        
main :: IO ()
main = do file <- getLine
          splitLines <- Common.readAndSplit file
          print(solve1 splitLines)
          print(solve2 splitLines)