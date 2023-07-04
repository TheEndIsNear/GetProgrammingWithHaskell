type FirstName = String
type MiddleName = String
type LastName = String
type BandName = String

data Book = Book {
     author :: Creator
   , isbn   :: String
   , title  :: String
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
     name :: String
   , description :: String
   , toyPrice :: Double
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

hpLoveCraft :: Creator
hpLoveCraft = AuthorCreator
                (Author
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"
