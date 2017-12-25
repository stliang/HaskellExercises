newtype Name =
  Name String
  deriving (Show)

newtype Acres =
  Acres Int
  deriving (Show)

data FarmerType
  = DairyFarmer
  | WhearFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer =
  Farmer Name
         Acres
         FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  } deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

data Automobile
  = Null
  | Car { make :: String
       ,  model :: String
       ,  year :: Integer}
  deriving (Eq, Show)

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = True

convert' :: Quantum -> Bool
convert' Yes = True
convert' No = True
convert' Both = False

convert'' :: Quantum -> Bool
convert'' Yes = True
convert'' No = False
convert'' Both = True

convert''' :: Quantum -> Bool
convert''' Yes = False
convert''' No = True
convert''' Both = True

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = True
convert5 No = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True


