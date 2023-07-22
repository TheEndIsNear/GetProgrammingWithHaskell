module Lib where

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = myHead xs : myTake (n-1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

mySaferTake :: Int -> Maybe [a] -> Maybe [a]
mySaferTake 0 _ = Just []
mySaferTake n (Just xs) = (:) <$> maybeHead xs
                              <*> mySaferTake (n-1) (Just (tail xs))
myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:_) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "dog"

charExampleEmpty :: [Char]
charExampleEmpty = ""

primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceeds limits of prime checker"
  show InvalidValue = "Numbers less than 2 are not candidates for primes"

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

