module Primes where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2 .. 10000]

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                    then next:unsafePrimeFactors
                                      (n `div` next) (next:primes)
                                    else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes

