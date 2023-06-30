data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAphabet
        rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
  where sizeOfAlphabet = 1 + largestCharNumber

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot4l
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN alphaSize
