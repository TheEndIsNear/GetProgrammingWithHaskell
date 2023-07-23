module Main (main) where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

data Book = Book 
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name

myBook :: Book
myBook = Book { author = "Will Kurt"
              , title = "Learn Haskell"
              , year = 2017
              }

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Serhii Plokhy\",\"title\":\"The Russo-Ukrainian War\",\"year\":2023}"

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Serhii Plokhy\",\"title\":\"The Russo-Ukrainian War\",\"year\":2023}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object [ "message" .= message
           , "error" .= errorCode
           ]

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

main :: IO ()
main = print "Hi"
