type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type Weight = Int
type PatientName = (String,String)
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
data Sex = Male | Female
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
data Patient = Patient {name :: Name
                       , sex :: Sex
                       , age :: Age
                       , height :: Height
                       , weight :: Weight
                       , bloodType :: BloodType }

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patientName age height = name ++ " " ++ ageHeight
  where name = lastName patientName ++ ", " ++ firstName patientName
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)" 

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

showAge :: Age -> String
showAge = show

showHeight :: Height -> String
showHeight height = show height ++ "in."

showWeight :: Weight -> String
showWeight weight = show weight ++ "lbs."

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRH :: RhType -> String
showRH Pos = "+"
showRH Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRH rh

canDonateTo ::  BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- universal donor
canDonateTo _ (BloodType AB _) = True -- universal receiver
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _  _ = False -- otherwise

canDonate :: Patient -> Patient -> Bool
canDonate p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeElizabethSmith :: Patient
janeElizabethSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 25 63 135 (BloodType O Neg)

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

patientSummary :: Patient -> String
patientSummary patient =
  "********\n"
  ++ "Patient Name: " ++ showName (name patient) ++ "\n"
  ++ "Sex: " ++ showSex (sex patient) ++ "\n"
  ++ "Age: " ++ showAge (age patient) ++ "\n"
  ++ "Height: " ++ showHeight (height patient) ++ "\n"
  ++ "Weight: " ++ showWeight (weight patient) ++ "\n"
  ++ "Blood Type: " ++ showBloodType (bloodType patient)
  
