import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

main :: IO ()
main = do
  userInput <- LIO.getContents
  let numbers = toInts userInput
  LIO.putStrLn $ (L.pack . show . sum) numbers

toInts :: L.Text -> [Int]
toInts  =  map (read . L.unpack) . L.lines

