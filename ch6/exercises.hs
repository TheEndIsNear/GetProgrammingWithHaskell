myRepeat n = cycle [n]

subseq a b = drop a . take b 

inFirstHalf x xs = x `elem` firstHalf
  where firstHalf = take (length xs `div` 2) xs
