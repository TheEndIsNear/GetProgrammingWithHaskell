import qualified Data.Map as Map
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

orgainPairs :: [(Int,Organ)]
orgainPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList orgainPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                      (\x -> x == Just organ)
                                      available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Num a => Maybe a -> a
numOrZero Nothing = 0
numOrZero (Just x) = x
