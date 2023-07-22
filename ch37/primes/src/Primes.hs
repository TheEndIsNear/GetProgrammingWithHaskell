module Primes where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2 .. 10000]

isPrime :: Int -> Maybe Bool
isPrime n | n < 0 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (n `elem` primes)
