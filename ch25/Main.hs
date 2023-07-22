module Main where

import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Glitcher

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename
  glitched <- foldM (\bytes fun -> fun bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_",filename]
  BC.writeFile glitchedFileName glitched
  print "All done"

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReplaceByte
                ,randomSortSection
                ,randomReplaceByte
                ,randomSortSection
                ,randomReverseSection
                ,randomReplaceByte]
