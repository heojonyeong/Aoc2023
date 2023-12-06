import qualified Common

data Quadratic = Quadratic { p::Double, q::Double }
    deriving(Show)
    
solveQuadratic :: Quadratic -> [Double]
solveQuadratic Quadratic {p=p, q=q} = solutions
    where a = (-p)/2
          b = a*a - q
          solutions = if a<0
                      then []
                      else [a - sqrt b, a + sqrt b]

parseQuadratics :: [String] -> [Quadratic]
parseQuadratics [timeLine, distanceLine] = quadratics
    where times :: [Double] =  map read (Common.splitAtMultiple ' ' (Common.dropPrefix "Time:" timeLine))
          distances :: [Double] = map read (Common.splitAtMultiple ' ' (Common.dropPrefix "Distance:" distanceLine))
          pairs = zip times distances
          quadratics = map (\(time, distance) -> Quadratic (-time) (distance+0.01)) pairs

parseQuadratic2 :: [String] -> Quadratic
parseQuadratic2 [timeLine, distanceLine] = quadratic
    where time :: Double = read (Common.dropAll ' ' (Common.dropPrefix "Time:" timeLine))
          distance :: Double = read (Common.dropAll ' ' (Common.dropPrefix "Distance:" distanceLine))
          quadratic = Quadratic (-time) (distance+0.001)

possibilities :: Quadratic -> Integer
possibilities q = (floor hi - ceiling lo) + 1
    where [lo,hi] = solveQuadratic q

solutions :: [Quadratic] -> [[Double]]
solutions = map solveQuadratic

solve1 :: [String] -> Integer
solve1 x = product poss
    where qs = parseQuadratics x
          poss = map possibilities qs

solve2 :: [String] -> Integer
solve2 x = possibilities $ parseQuadratic2 x

main = do file <- getLine
          splitLines <- Common.readAndSplit file
          print(solve1 splitLines)
          print(solve2 splitLines)