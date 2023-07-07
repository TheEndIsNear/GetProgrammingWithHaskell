import qualified Data.Map as Map

data Box a = Box a deriving Show
data Triple a = Triple a a a deriving Show
data List a = Empty | Cons a (List a) deriving Show
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

type Point3D = Triple Double
type FullName = Triple String
type Initials = Triple Char

wrap :: a -> Box a
wrap = Box 

unwrap :: Box a -> a
unwrap (Box x) = x

boxMap :: (a -> b) -> [Box a] -> [Box b]
boxMap _ [] = []
boxMap f (x:rest) = (:) (wrap $ f $ unwrap x)  (boxMap f rest)

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

tripleMap :: (a -> b) -> [Triple a] -> [Triple b]
tripleMap _ [] = []
tripleMap f (Triple x y z:rest) = (:) (Triple (f x) (f y) (f z)) (tripleMap f rest)

builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'a' (Cons 'a' (Cons 't' Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons a rest) = Cons (f a) (ourMap f rest)

itemCount1 :: (String,Int)
itemCount1 =  ("Erasers",25)

itemCount2 :: (String,Int)
itemCount2 = ("Pencils",25)

itemCount3 :: (String,Int)
itemCount3 = ("Pens",13)

itemInventory :: [(String,Int)]
itemInventory = [itemCount1,itemCount2,itemCount3]

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

orgainPairs :: [(Int,Organ)]
orgainPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList orgainPairs
