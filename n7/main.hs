import qualified Common
import qualified Common
import Data.List

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
    deriving(Show, Eq, Ord)

readCard :: Char -> Card
readCard '2' = Two
readCard '3' = Three
readCard '4' = Four
readCard '5' = Five
readCard '6' = Six
readCard '7' = Seven
readCard '8' = Eight
readCard '9' = Nine
readCard 'T' = T
readCard 'J' = J
readCard 'Q' = Q
readCard 'K' = K
readCard 'A' = A

data Hand = Empty
            | Single1 [Card] Card

            | Single2 [Card] Card Card
            | Pair1 [Card] Card

            | Single3 [Card] Card Card Card
            | Pair1Single1 [Card] Card Card
            | Triple1 [Card] Card

            | Single4 [Card] Card Card Card Card
            | Pair1Single2 [Card] Card Card Card
            | Pair2 [Card] Card Card
            | Triple1Single1 [Card] Card Card
            | Quadruple1 [Card] Card

            | Single5 [Card] Card Card Card Card Card
            | Pair1Single3 [Card] Card Card Card Card
            | Pair2Single1 [Card] Card Card Card
            | Triple1Single2 [Card] Card Card Card
            | Triple1Pair1 [Card] Card Card
            | Quadruple1Single1 [Card] Card Card
            | Quintuple [Card] Card
 deriving(Show, Eq, Ord)

parseHand :: (Hand, [Card]) -> (Hand, [Card])
parseHand (h,[]) = (h,[])
parseHand (h, x:xs) = case h of
                         Empty -> parseHand (Single1 [x] x, xs)
                         Single1 a b -> if b==x
                                        then parseHand (Pair1 (a ++ [x]) b, xs)
                                        else parseHand (Single2 (a ++ [x]) b x, xs)
                         Single2 a b c -> if b==x
                                          then parseHand (Pair1Single1 (a ++ [x]) b c, xs)
                                          else if c==x
                                               then parseHand (Pair1Single1 (a ++ [x]) c b, xs)
                                               else parseHand (Single3 (a ++ [x]) b c x, xs)
                         Pair1 a b -> if b==x
                                      then parseHand (Triple1 (a ++ [x]) b, xs)
                                      else parseHand (Pair1Single1 (a ++ [x]) b x, xs)
                         Single3 a b c d -> if b==x
                                            then parseHand (Pair1Single2 (a ++ [x]) b c d, xs)
                                            else if c==x 
                                                 then parseHand (Pair1Single2 (a ++ [x]) c b d, xs)
                                                 else if d==x
                                                      then parseHand (Pair1Single2 (a ++ [x]) d b c, xs)
                                                      else parseHand (Single4 (a ++ [x]) b c d x, xs)
                         Pair1Single1 a b c -> if b==x
                                               then parseHand (Triple1Single1 (a ++ [x]) b c, xs)
                                               else if c==x
                                                    then parseHand (Pair2 (a ++ [x]) b c, xs)
                                                    else parseHand (Pair1Single2 (a ++ [x]) b c x, xs)
                         Triple1 a b -> if b==x
                                        then parseHand (Quadruple1 (a ++ [x]) b, xs)
                                        else parseHand (Triple1Single1 (a ++ [x]) b x, xs)
                         Single4 a b c d e -> if b==x
                                              then parseHand (Pair1Single3 (a ++ [x]) b c d e, xs)
                                              else if c==x
                                                   then parseHand(Pair1Single3 (a ++ [x]) c b d e, xs)
                                                   else if d==x
                                                        then parseHand(Pair1Single3 (a ++ [x]) d b c e, xs)
                                                        else if e==x
                                                             then parseHand (Pair1Single3 (a ++ [x]) e b c d, xs)
                                                             else parseHand (Single5 (a ++ [x]) b c d e x, xs)
                         Pair1Single2 a b c d -> if b==x
                                                 then parseHand (Triple1Single2 (a ++ [x]) b c d, xs)
                                                 else if c==x
                                                      then parseHand (Pair2Single1 (a ++ [x]) b c d, xs)
                                                      else if d==x
                                                           then parseHand (Pair2Single1 (a ++ [x]) b d c, xs)
                                                           else parseHand (Pair1Single3 (a ++ [x]) b c d x, xs)
                         Pair2 a b c -> if b==x
                                        then parseHand (Triple1Pair1 (a ++ [x]) b c, xs)
                                        else if c==x
                                             then parseHand (Triple1Pair1 (a ++ [x]) c b, xs)
                                             else parseHand (Pair2Single1 (a ++ [x]) b c x, xs)
                         Triple1Single1 a b c -> if b==x
                                                 then parseHand (Quadruple1Single1 (a ++ [x]) b c, xs)
                                                 else if c==x
                                                      then parseHand (Triple1Pair1 (a ++ [x]) b c, xs)
                                                      else parseHand (Triple1Single2 (a ++ [x]) b c x, xs)
                         Quadruple1 a b -> if b==x
                                           then parseHand (Quintuple (a ++ [x]) b, xs)
                                           else parseHand (Quadruple1Single1 (a ++ [x]) b x, xs)

parseHands :: [String] -> [(Hand, Integer)]
parseHands = map parseHand_
    where parseHand_ y = (hand, bet)
           where (handS, betS) = Common.splitAt ' ' y
                 cards = map readCard handS
                 bet :: Integer = read betS
                 hand = fst $ parseHand (Empty, cards)

solve1 :: [String] -> Integer
solve1 x = value
    where hands = sort $ parseHands x
          accum :: [(Hand, Integer)] -> Integer -> Integer -> Integer
          accum [] _ total = total
          accum ((a,b):ys) i total = accum ys (i+1) (total+b*i) 

          value = accum hands 1 0

main = do file <- getLine
          input <- Common.readAndSplit file
          print $ solve1 input