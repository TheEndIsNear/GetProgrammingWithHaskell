import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename
  glitched1 <- randomSortSection imageFile
  glitched2 <- randomSortSection glitched1
  glitched3 <- randomSortSection glitched2
  glitched4 <- randomSortSection glitched3
  glitched5 <- randomReplaceByte glitched4
  let glitchedFileName = mconcat ["glitched_",filename]
  BC.writeFile glitchedFileName glitched5
  print "All done"

intToChar :: Int -> Char
intToChar int = toEnum safeChar
  where safeChar = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before,newChar,after]
  where (before,rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1,bytesLength)
  charVal <- randomRIO (0,255)
  return (replaceByte location charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
  where (before,rest) = BC.splitAt start bytes
        (target,after) = BC.splitAt size rest
        changed = BC.reverse $ BC.sort target

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0,bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)
