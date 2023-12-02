import Common qualified

data RGB = RGB {red::Color, green::Color, blue::Color}
    deriving (Show)

maximum :: RGB -> RGB -> RGB
maximum (RGB (Red r1) (Green g1) (Blue b1)) (RGB (Red r2) (Green g2) (Blue b2)) = RGB (Red (max r1 r2)) (Green (max g1 g2)) (Blue (max b1 b2))

plus :: RGB -> RGB -> RGB
(RGB (Red r1) (Green g1) (Blue b1)) `plus` (RGB (Red r2) (Green g2) (Blue b2)) = RGB (Red (r1+r2)) (Green (g1+g2)) (Blue (b1+b2))
 
data Color = Red Integer
             | Green Integer
             | Blue Integer

    deriving (Show)

dropPrefix :: String -> String -> String
dropPrefix [] x  = x
dropPrefix x [] = []
dropPrefix (x:xs) (y:ys) = if x == y
                           then dropPrefix xs ys
                           else y:ys

splitAt :: Char -> String -> (String, String)
splitAt c [] = ([], [])
splitAt c (x:xs) = if c == x
                   then ([], xs)
                   else (x: fst rest, snd rest)

                   where rest = Main.splitAt c xs

splitAtRepeating :: Char -> String -> [String]
splitAtRepeating c [] = []
splitAtRepeating c x = first : splitAtRepeating c rest
    where (first, rest) = Main.splitAt c x

parseGame :: String -> (Integer, [[String]])
parseGame x = (id, cleaned)
    where id = read (dropPrefix "Game " gamePart) :: Integer
          cleaned = map (map (dropPrefix " ")) splitRounds

          splitRounds = map (splitAtRepeating ',') rounds
          rounds = splitAtRepeating ';' roundsPart
          (gamePart, roundsPart) = Main.splitAt ':' x

addColor :: Color -> RGB -> RGB
addColor (Red x) (RGB (Red r) (Green g) (Blue b)) = RGB (Red (x + r)) (Green g) (Blue b)
addColor (Green x) (RGB (Red r) (Green g) (Blue b)) = RGB (Red r) (Green (x + g)) (Blue b)
addColor (Blue x) (RGB (Red r) (Green g) (Blue b)) = RGB (Red r) (Green g) (Blue (x + b))

toRGB :: [Color] -> RGB
toRGB = foldr addColor (RGB (Red 0) (Green 0) (Blue 0))

maxRGB :: [RGB] -> RGB
maxRGB = foldr Main.maximum (RGB (Red 0) (Green 0) (Blue 0))

endsWith :: String -> String -> Bool
endsWith x [] = True
endsWith [] x = False
endsWith (x:xs) (y:ys) = if x == y
                         then endsWith xs ys
                         else endsWith xs (y:ys)

makeColor :: String -> Color
makeColor x | endsWith x "green" = Green number
            | endsWith x "red" = Red number
            | endsWith x "blue" = Blue number
            
            where number = (read :: String -> Integer) (fst (Main.splitAt ' ' x))

makeRGB :: [String] -> RGB
makeRGB = foldr (addColor . makeColor) (RGB (Red 0) (Green 0) (Blue 0)) 

solveGameHelper :: String -> (Integer, RGB)
solveGameHelper x = (id, largestRGB)
    where (id, splits) = parseGame x
          colors = [[makeColor c | c <- components] | components <- splits] 
          rgbs = map toRGB colors
          largestRGB = maxRGB rgbs

validatedNum :: (Integer, RGB) -> Integer -> Integer -> Integer -> Integer
validatedNum (id, RGB (Red r1) (Green g1) (Blue b1)) maxR maxG maxB = if r1 <= maxR && g1 <= maxG && b1 <= maxB
                                                                      then id
                                                                      else 0

solveGame :: Integer -> Integer -> Integer -> String -> Integer
solveGame maxR maxG maxB x = validatedNum (solveGameHelper x) maxR maxG maxB

solveBound :: String -> Integer
solveBound = solveGame 12 13 14

solve :: [String] -> Integer
solve = foldr ((+) . solveBound) 0

power :: (Integer, RGB) -> Integer
power (_, RGB (Red r) (Green g) (Blue b)) = r*g*b

solveBound2 :: String -> Integer
solveBound2 = power . solveGameHelper

solve2 :: [String] -> Integer
solve2 = foldr ((+) . solveBound2) 0

main = do --print(endsWith "5 blue" "blue")
    --      print(makeColor "123 blue")
   --       print(makeRGB ["123 blue", "22 blue", "5 green", "7 red", "3 green"])
 --         print(parseGame "Game 1: 3 red; 5 red, 2 blue; 3 blue, 2 red, 15 green")
          splitLines <- Common.readAndSplit "input.txt"
          print(solve splitLines)
          print(solve2 splitLines)