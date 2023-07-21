askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStateMent :: String -> String
nameStateMent name = "Hello " ++ name ++ "!"

main :: IO ()
main = askForName >>
       getLine >>=
         (\name ->
           return $ nameStateMent name) >>=
       putStrLn
