import qualified Common
import Data.List

filterStrings :: String -> [String] -> [String]
filterStrings e [] = []
filterStrings e (x:xs) = if e == x
                         then filterStrings e xs
                         else x : filterStrings e xs

seeds :: String -> [Integer]
seeds x = map read (filterStrings "" (Common.splitAtRepeating ' ' (snd $ Common.splitAt ':' x)))

toMap :: [String] -> [(Integer, Integer, Integer)]
toMap [] = []
toMap [""] = []
toMap (x:xs) = if x /= ""
               then interval : toMap xs
               else []
    where interval = case nums of
                        [a,b,c] -> (a,b,c)
          nums = map read (Common.splitAtRepeating ' ' x)

mapped :: Integer -> [(Integer, Integer, Integer)] -> Integer
mapped a [] = a
mapped a ((dest, src, length):xs) = if a >= src && a <= (src+length)
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

main :: IO ()
main = do file <- getLine
          splitLines <- Common.readAndSplit file
          print(solve1 splitLines)