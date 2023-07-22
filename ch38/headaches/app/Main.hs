module Main (main) where

import Lib (isPrime,displayResult)

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print $ displayResult result

