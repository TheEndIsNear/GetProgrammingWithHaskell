import Control.Monad

main :: IO ()
main = do
  input <- getContents
  let reversed = reverse input
  print reversed
