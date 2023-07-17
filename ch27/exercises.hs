data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box val) = Box (f val)

morePresents :: Box a -> Box [a]
morePresents box = replicate 5 <$> box

myBox :: Box Int
myBox = Box 1

wrapped :: Box a -> Box (Box a)
wrapped = fmap Box

unwrap :: Box a -> a
unwrap (Box val) = val
