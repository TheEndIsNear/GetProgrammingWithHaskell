import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.Char(isPunctuation, isSpace, toLower)
import qualified Data.Text as T

prop_punctuationInvarient :: T.Text -> Bool
prop_punctuationInvarient text = preprocess text ==
                                 preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reversetInvariant :: T.Text -> Bool
prop_reversetInvariant text = isPalindrome text == (isPalindrome . T.reverse) text

prop_whitespaceInvarient :: T.Text -> Bool
prop_whitespaceInvarient text = preprocess text ==
                                preprocess noWhitespaceText
  where noWhitespaceText = T.filter (not . isSpace) text

prop_capitalizationInvarient :: T.Text -> Bool
prop_capitalizationInvarient text = preprocess text ==
                                    preprocess noCapText
  where noCapText = T.map toLower text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000} prop_punctuationInvarient
  quickCheckWith stdArgs {maxSuccess = 1000} prop_reversetInvariant
  quickCheckWith stdArgs {maxSuccess = 1000}  prop_whitespaceInvarient
  quickCheckWith stdArgs {maxSuccess = 1000}  prop_capitalizationInvarient
  putStrLn "done!"
