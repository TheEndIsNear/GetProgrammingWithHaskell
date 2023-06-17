myGCD a b = if remainder == 0
               then b
               else myGCD b remainder
  where remainder = a `rem` b

myGCD2 a 0 = a
myGCD2 a b = myGCD2 b (a `rem` b)

sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  _ -> "a bunch"

myHead (x:_) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = []
