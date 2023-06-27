import Data.Char(toLower)
myElem x xs = filteredLength > 0
  where filteredLength = length $ filter (== x) xs

isPalindrome xs = (==) sentence reverseSentence
  where sentence = map toLower $ filter (/= ' ') xs
        reverseSentence = reverse sentence

harmonic n = foldr (+) $ map ((/) 1) [1..n]
