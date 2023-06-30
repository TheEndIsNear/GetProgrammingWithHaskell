data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

newtype Name = Name (String, String) deriving (Eq)

names :: [Name]
names = [ Name ("Emil", "Cioran")
        , Name ("Eugene", "Thacker")
        , Name ("Friedrick", "Nietzsche")]

instance Ord Name where
  compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1, f1) (l2, f2)

instance Show Name where
  show (Name (f,l)) = f ++ " " ++ l
