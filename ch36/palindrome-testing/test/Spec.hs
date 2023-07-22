import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char(isPunctuation)
import Data.Text as T

prop_punctuationInvarient :: T.Text -> Bool
prop_punctuationInvarient text = preprocess text ==
                                 preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reversetInvariant :: T.Text -> Bool
prop_reversetInvariant text = isPalindrome text == (isPalindrome . T.reverse) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000} prop_punctuationInvarient
  quickCheck prop_reversetInvariant
  putStrLn "done!"
