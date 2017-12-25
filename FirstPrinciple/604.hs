import Data.Int

data Price =
  Price Integer
  deriving (Eq, Show)

data Size =
  Feet Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data AirLine
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane AirLine
          Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000) -- :: Vehicle

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Feet 100)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man

data PugType =
  PugData

data Example =
  MakeExample Int
  deriving (Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n < 42

newtype Goats =
  Goats Int
  deriving (Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 43

newtype Tu =
  Tu (Int, String)

newtype Fields =
  Fields (Int, Int)

instance TooMany Tu where
  tooMany (Tu (n, _)) = n > 43

instance TooMany Fields where
  tooMany (Fields (x, y)) = x + y > 43

newtype Cus a =
  Cus (a, a)

instance (Num a, Ord a, TooMany a) =>
         TooMany (Cus a) where
  tooMany (Cus (x, y)) = x + y > 43

data BigSmall
  = Big Bool -- 2
  | Small Bool -- 2
  deriving (Eq, Show)

data NumberOrBool
  = Numba Int8 -- 256
  | BoolyBool Bool -- 2
  deriving (Eq, Show)

type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

data GuessWhat =
  Checkbutt
  deriving (Eq, Show)

data Id a =
  MkId a
  deriving (Eq, Show)

data Product a b =
  Product a
          b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  } deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow
            NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow
               NumPig
               NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name
          Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name
          Age
          LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name
            Age
            PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = Checkbutt

data OperatingSystem
  = GnuPlusLinux
  | OpenDSBPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Enum, Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Enum, Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = GnuPlusLinux}

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenDSBPlusNevermindJustBSDStill, Mac, Windows]

allOperatingSystems' :: [OperatingSystem]
allOperatingSystems' = enumFrom (toEnum 0 :: OperatingSystem)

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allLanguages' :: [ProgLang]
allLanguages' = enumFrom (toEnum 0 :: ProgLang)

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = o, lang = l} | o <- allOperatingSystems', l <- allLanguages']

data ThereYet =
  There Float
        Int
        Bool
  deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

