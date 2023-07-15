import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename
  glitched <- return imageFile
  let glitchedFileName = mconcat ["glitched_",filename]
  BC.writeFile glitchedFileName glitched
  print "All done"
