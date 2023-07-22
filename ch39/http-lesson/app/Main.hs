module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import System.Environment
import Network.HTTP.Simple

main :: IO ()
main = do
  token <- BC.pack <$> getEnv "NOAA_TOKEN"
  let request = buildRequest token noaaHost requestType apiPath
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else print "request failed with error"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

requestType :: BC.ByteString
requestType = "GET"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
                                $ setRequestHost host
                                $ setRequestHeader "token" [token]
                                $ setRequestPath path
                                $ setRequestSecure True
                                $ setRequestPort 443
                                $ defaultRequest

