module Main (main) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data User = User
  { userId :: Int
  , userName :: String
  }
instance Show User where
  show user = mconcat [show $ userId user
                      , ".) "
                      , userName user]
instance FromRow User where
  fromRow = User <$> field
                 <*> field

data Tool = Tool
  { toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
  }
instance Show Tool where
  show tool = mconcat [ show $ toolId tool
                      , ".)"
                      , name tool
                      , "\n description: "
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n"]
instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

addUser :: String -> IO ()
addUser userName = do
  withConn "tools.db" $
    \conn -> do
      execute conn "INSERT INTO users (username) VALUES (?)"
        (Only userName)
      print "user added"

checkout :: Int -> Int -> IO ()
checkout userId toolID = do
  withConn "tools.db" $
    \conn -> do
      execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
        (userId, toolID)

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

printUsers :: IO ()
printUsers = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat ["SELECT * FROM tools "
                                          , "WHERE id NOT IN "
                                          , "(SELECT tool_id FROM checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $ mconcat ["SELECT * FROM tools "
                                           , "WHERE id IN "
                                           , "(SELECT tool_id FROM checkedout);"]

main :: IO ()
main = print "db-lesson"
