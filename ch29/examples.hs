doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,20000]

boxMultiplier :: [Int]
boxMultiplier = [10,50]

totalPrize :: [Int]
totalPrize = (+) <$> doorPrize <*> boxPrize

multipliedPrize :: [Int]
multipliedPrize = (*) <$> totalPrize <*> boxMultiplier

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2 .. n]
        composite = pure (*) <*> [2 .. n] <*> [2 .. n]
        isNotComposite = not . (`elem` composite)

data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"
            ,"Matt Willy"]

testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User <*> testNames
                        <*> testIds
                        <*> testScores
