import Common qualified

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

replaceNumberWords :: String -> String
replaceNumberWords ('o':'n':'e':xs) = '1':replaceNumberWords ('e':xs)
replaceNumberWords ('t':'w':'o':xs) = '2':replaceNumberWords ('o':xs)
replaceNumberWords ('t':'h':'r':'e':'e':xs) = '3':replaceNumberWords ('e':xs)
replaceNumberWords ('f':'o':'u':'r':xs) = '4':replaceNumberWords xs
replaceNumberWords ('f':'i':'v':'e':xs) = '5':replaceNumberWords ('e':xs)
replaceNumberWords ('s':'i':'x':xs) = '6':replaceNumberWords xs
replaceNumberWords ('s':'e':'v':'e':'n':xs) = '7':replaceNumberWords ('n':xs)
replaceNumberWords ('e':'i':'g':'h':'t':xs) = '8':replaceNumberWords ('t':xs)
replaceNumberWords ('n':'i':'n':'e':xs) = '9':replaceNumberWords ('e':xs)
replaceNumberWords (x:xs) = x:replaceNumberWords xs
replaceNumberWords [] = []

firstDigit :: String -> Integer
firstDigit [] = 0
firstDigit [x] = if isDigit x
                 then charToInt x
                 else 0
firstDigit (x:xs) = if isDigit x
                    then charToInt x
                    else firstDigit xs

lastDigit :: String -> Integer
lastDigit = firstDigit . reverse

extractNumFromLine :: String -> Integer
extractNumFromLine [] = 0
extractNumFromLine [x] = 0
extractNumFromLine x = firstDigit modified * 10 + lastDigit modified
    where modified = replaceNumberWords x

main = do
    lines <- Common.readAndSplit "input.txt"
    let numbers = [extractNumFromLine line | line <- lines]
    print (sum numbers)
   -- print [(line, replaceNumberWords line) | line <- lines]
