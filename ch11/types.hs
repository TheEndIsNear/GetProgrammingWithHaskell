y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99,0.7,0.8]

letters :: [Char]
letters = ['a','b','c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34,73)

firstLastMiddle :: (String,String,Char)
firstLastMiddle = ("Oscar","Grouch",'D')

streetAddress :: (Int,String)
streetAddress = (123,"Happy St.")

half :: Int -> Double
half n = (fromIntegral n) / 2

printDouble :: Int -> String
printDouble = show . (*) 2

halve :: Integer -> Integer
halve value = value `div` 2

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number,street,town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)
