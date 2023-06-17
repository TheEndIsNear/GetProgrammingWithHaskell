calcChange :: (Num a, Ord a) => a -> a -> a
calcChange owed given = if change > 0
                           then change
                           else 0
  where change = given - owed

doublePlusTwo :: Num a => a -> a
doublePlusTwo x = doubleX + 2
  where doubleX = x + x

simple :: a -> a
simple x = x

inc :: Num a => a -> a
inc = (+) 1

double ::Num a => a -> a
double = (*) 2

square :: Num a => a -> a
square = flip (^) 2

function n = if even n
                then n - 2
                else 3 * n + 1
