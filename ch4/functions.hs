import Data.List

ifEven myFunction x = if even x
                         then myFunction x
                         else x

ifEvenInc = ifEven $ (+) 1
ifEvenDouble = ifEven $ (*) 2
ifEvenSquare = ifEven $ flip (^) 2
ifEvenCubed = ifEven $ flip (^) 3

names = [("Ian", "Curtis"),
         ("Bernard", "Sumber"),
         ("Peter", "Hook"),
         ("Stephen", "Morris"),
         ("Dave", "Morris")]

compareLastNames name1 name2 = case nameCompare of
  EQ  -> compare firstName1 firstName2
  _ -> nameCompare
  where lastName1 = snd name1
        lastname2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2
        nameCompare = compare lastName1 lastname2

addressLetter :: (String, String) -> String -> String
addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

sfOffice :: (String, String) -> String
sfOffice name = if lastName < "L"
                   then nameText
                       ++ " - PO Box 1234 - San Francisco, CA, 94111"
                   else nameText
                       ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where nameText = fst name ++ " " ++ snd name
        lastName = snd name

nyOffice :: (String, String) -> String
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice :: (String, String) -> String
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

getLocationFunction :: String -> ((String, String) -> String)
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> fst name ++ " " ++ snd name)

flipBinaryArguments :: (a -> b -> c) -> (b -> a -> c)
flipBinaryArguments binaryFunction x y = binaryFunction y x

addressLetterV2 = flipBinaryArguments addressLetter

addressLetterNY = addressLetterV2 "ny"
