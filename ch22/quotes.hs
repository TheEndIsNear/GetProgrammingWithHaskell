import Control.Monad
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- getLine
  let quote = Map.lookup input quoteData
  displayQuote quote

quoteData :: Map.Map String String
quoteData = Map.fromList [("1", "Knowledge is power."), ("2", "Life is like a box of chocolates. You never know what you're gonna get."), ("3", "Life is like riding a bicycle. To keep your balance, you must keep moving."), ("4", "May the Force be with you."), ("5", "All that glitters is not gold.")]

displayQuote :: Maybe String -> IO ()
displayQuote (Just x) = print x
displayQuote Nothing = print "Quote not found"
