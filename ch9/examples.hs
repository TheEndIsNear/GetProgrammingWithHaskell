import Data.Char(toLower)
myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = filteredLength > 0
  where filteredLength = length $ filter (== x) xs

isPalindrome :: [Char] -> Bool
isPalindrome xs = (==) sentence reverseSentence
  where sentence = map toLower $ filter (/= ' ') xs
        reverseSentence = reverse sentence

harmonic :: (Fractional a, Enum a) => a -> a
harmonic n = foldr (+) 0 $ map ((/) 1) [1..n]
