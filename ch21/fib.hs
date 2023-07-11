main :: IO ()
main = do
  putStrLn "Enter a number for the fibonacci number"
  n <- getLine
  let val = fib (read n)
  let statement = mconcat ["The fibonacci of ",n," is: ", show val]
  putStrLn statement

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

