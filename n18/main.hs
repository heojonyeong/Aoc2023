import qualified Common

readHex :: String -> Int
readHex [] = 0
readHex (x:xs) = readHex_ (x:xs) 0
    where readHex_ [] t = t
          readHex_ (x:xs) t = readHex_ xs (t*16 + digit) 
            where
              digit | x=='1' = 1
                    | x=='2' = 2
                    | x=='3' = 3
                    | x=='4' = 4
                    | x=='5' = 5
                    | x=='6' = 6
                    | x=='7' = 7
                    | x=='8' = 8
                    | x=='9' = 9
                    | x=='0' = 0
                    | x=='a' || x=='A' = 10
                    | x=='b' || x=='B' = 11
                    | x=='c' || x=='C' = 12
                    | x=='d' || x=='D' = 13
                    | x=='e' || x=='E' = 14
                    | x=='f' || x=='F' = 15

parse1 :: String -> (Char, Int)
parse1 l = (d,num)
    where [[d], n, _] = Common.splitAtMultiple ' ' l
          num = read n

parse2 :: String -> (Char, Int)
parse2 l = (dir,n_)
    where [_,_, '(':'#':code] = Common.splitAtMultiple ' ' l
          n_ = readHex (take 5 code)
          d_ = code !! 5
          dir | d_ == '0' = 'R'
              | d_ == '1' = 'D'
              | d_ == '2' = 'L'
              | d_ == '3' = 'U'

solve :: [String] -> (String -> (Char,Int)) -> Int -> Int -> Int -> Int -> Int
solve [] _ _ _ area boundary = boundary + (area - (boundary `div` 2) + 1) --pick formula
solve (l:ls) parse yStart yCur area boundary | d == 'D' = solve ls parse yStart (yCur-n) area (boundary+n)
                                             | d == 'U' = solve ls parse yStart (yCur+n) area (boundary+n)
                                             | otherwise = solve ls parse yStart yCur (area+sign*nArea) (boundary+n)
    where (d,n) = parse l
          nArea = n * (yCur - yStart)
          sign | d == 'L' = -1
               | d == 'R' =  1

solve1 :: [String] -> Int
solve1 x = solve x parse1 0 0 0 0

solve2 :: [String] -> Int
solve2 x = solve x parse2 0 0 0 0

main = do
    file <- getLine
    splitLines <- Common.readAndSplit file
    print $ solve1 splitLines
    print $ solve2 splitLines