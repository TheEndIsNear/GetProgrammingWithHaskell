myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

ackerman :: Int -> Int -> Int
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))

collatz :: Int -> Int
collatz 1 = 1
collatz n = if even n
  then  1 + collatz (n `div` 2)
  else  1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fastFib :: Int -> Int -> Int -> Int
fastFib n1 n2 0 = n1
fastFib 1 1 counter = fastFib 2 1 (counter - 3)
fastFib n1 n2 counter = fastFib (n1+n2) n1 (counter - 1)
