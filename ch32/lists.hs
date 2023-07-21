import Control.Monad

powersOfTwo :: Int -> [Int]
powersOfTwo n = [2^value | value <- [1 .. n]]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = [(powersOfTwo,powersOfThree)
                        | value <- [1 .. n]
                        , let powersOfTwo = 2 ^ value
                        , let powersOfThree = 3 ^ value]

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = [(evenValue,oddValue) | evenValue <- [2,4 .. n]
                                      , oddValue <- [1,3 .. n]]

pairsOfNumberSquares :: Int -> [(Int, Int)]
pairsOfNumberSquares n = do
  value <- [1 .. n]
  return (value,value^2)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard(even value)
  return value

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = do
  x <- xs
  guard(f x)
  return x

evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n^2
  guard(even nSquared)
  return nSquared

