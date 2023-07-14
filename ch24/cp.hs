import System.Environment 
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let [file1,file2] = args
  input <- readFile file1
  writeFile file2 input 
