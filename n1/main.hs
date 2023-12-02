import Common qualified

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
firstDigit [x] = if Common.isDigit x
                 then Common.charToInt x
                 else 0
firstDigit (x:xs) = if Common.isDigit x
                    then Common.charToInt x
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
