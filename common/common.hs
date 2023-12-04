module Common where
import Data.Array

readAndSplit :: String -> IO [String]
readAndSplit x = do file <- readFile x
                    return (lines file)

toArray :: [a] -> Array Int a
toArray [] = array (1,0) []
toArray x = listArray (0, length x - 1) x

to2DArray :: [[a]] -> Array (Int, Int) a
to2DArray [] = array ((1,0), (1,0)) []
to2DArray x = listArray ((0,0), (height-1, width-1)) flattened
    where width = length $ head x
          height = length x
          flattened = concat x

charToInt :: Char -> Integer
charToInt x | x == '1' = 1
            | x == '2' = 2
            | x == '3' = 3
            | x == '4' = 4
            | x == '5' = 5
            | x == '6' = 6
            | x == '7' = 7
            | x == '8' = 8
            | x == '9' = 9
            | otherwise = 0

isDigit :: Char -> Bool
isDigit x   | x == '1' = True
            | x == '2' = True
            | x == '3' = True
            | x == '4' = True
            | x == '5' = True
            | x == '6' = True
            | x == '7' = True
            | x == '8' = True
            | x == '9' = True
            | x == '1' = True 
            | x == '0' = True
            | otherwise = False

isSymbol :: Char -> Bool
isSymbol x = x `elem` "!@#$%^&*()-_=+/?"          

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix [] x  = x
dropPrefix x [] = []
dropPrefix (x:xs) (y:ys) = if x == y
                           then dropPrefix xs ys
                           else y:ys

splitAt :: Eq a => a -> [a] -> ([a], [a])
splitAt c [] = ([], [])
splitAt c (x:xs) = if c == x
                   then ([], xs)
                   else (x: fst rest, snd rest)

                   where rest = Common.splitAt c xs

-- Split at every occurence of c
splitAtRepeating :: Eq a => a -> [a] -> [[a]]
splitAtRepeating c [] = []
splitAtRepeating c x = first : splitAtRepeating c rest
    where (first, rest) = Common.splitAt c x

stripFirst :: Eq a => a -> [a] -> [a]
stripFirst x [] = []
stripFirst c (x:xs) = if c == x
                      then stripFirst c xs
                      else x:xs

-- Split at everz occurence of c but treat repeating occurences as single
splitAtMultiple :: Eq a => a -> [a] -> [[a]]
splitAtMultiple c [] = []
splitAtMultiple c x = first : splitAtMultiple c stripped 

    where (first, rest) = Common.splitAt c x
          stripped = stripFirst c rest

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x [] = True
endsWith [] x = False
endsWith (x:xs) (y:ys) = if x == y
                         then endsWith xs ys
                         else endsWith xs (y:ys)

