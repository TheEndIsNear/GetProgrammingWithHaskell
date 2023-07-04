type FirstName = String
type MiddleName = String
type LastName = String
type BandName = String

data Book = Book {
     author :: Creator
   , isbn   :: String
   , bookTitle  :: String
   , year   :: Int
   , bookPrice  :: Double
   }

data VinylRecord = VinylRecord {
     artist      :: Creator
   , recordTitle :: String
   , recordYear  :: String
   , recordPrice :: Double
   }
  
data CollectibleToy = CollectibleToy {
     name        :: String
   , toyDescription :: String
   , toyPrice    :: Double
   }

data Pamphlet = Pamphlet {
     title :: String
   , description :: String
   , contact :: String
   }

data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char deriving Show

data Creator = AuthorCreator Author | Artist Creator Artist deriving Show

data Author = Author Name deriving Show

data Artist = Person Name | Band BandName deriving Show

data StoreItem = BookItem Book 
  | RecordItem VinylRecord 
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

hpLoveCraft :: Creator
hpLoveCraft = AuthorCreator
                (Author
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"
