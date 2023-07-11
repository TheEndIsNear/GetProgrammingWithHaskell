import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

nameMap :: Map.Map Int String
nameMap = Map.fromList [(1,"Arnold"),(2,"Steve")]

maybeMain :: Maybe String
maybeMain = do
  name1 <- Map.lookup 1 nameMap
  name2 <- Map.lookup 2 nameMap
  let person1 = helloPerson name1
  let person2 = helloPerson name2
  return (mconcat [person1," ",person2])

