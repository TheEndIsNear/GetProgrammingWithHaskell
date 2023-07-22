import Lib

import Test.QuickCheck

import Data.Char 

prop_punctuationInvarient :: String -> Bool
prop_punctuationInvarient text = preprocess text ==
                                 preprocess noPuncText
  where noPuncText = filter (not . isPunctuation) text

prop_reversetInvariant :: String -> Bool
prop_reversetInvariant text = isPalindrome text == (isPalindrome . reverse) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000} prop_punctuationInvarient
  quickCheck prop_reversetInvariant
  putStrLn "done!"
