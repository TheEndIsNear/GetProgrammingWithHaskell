example :: Int
example = (*) ((+) 2 4) 6

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f val = f <$> val

--exampleMaybe :: Maybe Int
--exampleMaybe =  (*) ((+) <$> 2 <*> 4) 6

beersList :: [Int]
beersList = [6,12]

--beersLeft :: [Int]
beersLeft = (\x -> x - 4) <$> beersList

friendsComing :: [Int]
friendsComing = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> friendsComing

drinksPerPerson :: [Int]
drinksPerPerson = [3,4]

totalBeers :: [Int]
totalBeers = (-) <$> ((*) <$> totalPeople <*> drinksPerPerson) <*> beersLeft
