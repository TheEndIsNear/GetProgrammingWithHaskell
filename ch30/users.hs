import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int
type WillCoId = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                           ,(2,"KINGinYellow")
                           ,(3,"dagon1997")
                           ,(4,"rcatrter1919")
                           ,(5,"xCTHULHUx")
                           ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                           ,("KINGinYellow",15000)
                           ,("dagon1997",300)
                           ,("rcatrter1919",12)
                           ,("xCTHULHUx",50000)
                           ,("yogSOThoth",150000)]

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB 

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username
