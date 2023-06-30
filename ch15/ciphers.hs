data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAphabet
        rotation = offset `mod` alphabetSize
