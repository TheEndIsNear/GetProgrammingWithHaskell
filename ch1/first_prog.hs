main :: IO ()
main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title"
  title <- getLine
  print "Who is the Author"
  author <- getLine
  print (createEmail recipient title author)

toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: String -> String
bodyPart bookTitle = "Thanks for writing about " ++ bookTitle ++ "\n."

fromPart :: String -> String
fromPart author = "I will return an answer to your question as soon as I can.\n" ++ author

createEmail :: String -> String -> String -> String
createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author
