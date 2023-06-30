-- the function cannot work for making a safe version of head
--myHead :: [a] -> a | []
--myHead (x|_) = x
--myHead [] = x

myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail [] = []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
