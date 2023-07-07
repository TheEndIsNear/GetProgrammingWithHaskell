data Box a = Box a deriving Show
data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
type FullName = Triple String
type Initials = Triple Char

wrap :: a -> Box a
wrap = Box 

unwrap :: Box a -> a
unwrap (Box x) = x

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
