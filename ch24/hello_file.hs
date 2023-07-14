import System.IO

main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hasLine helloFile
  firstLine <- if not hasLine
                  then hGetLine helloFile
                  else return "empty"
  secondLine <- if not hasLine
                   then hGetLine helloFile
                   else return ""
  goobyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goobyeFile secondLine
  hClose helloFile
  hClose goobyeFile
  putStrLn "done!"

hasLine :: Handle -> IO Bool
hasLine = hIsEOF
