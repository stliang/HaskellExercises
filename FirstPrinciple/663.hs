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

data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

eQuad :: Either Quad Quad -- 4 + 4 = 8
eQuad = Right One

--prodQuad :: (Quad, Quad) -- 4 * 4 = 16
--funcQuad :: Quad -> Quad -- 4 ^ 4 = 256
--prodTBool :: (Bool, Bool, Bool) -- 2 * 2 * 2 = 8
--gTwo :: Bool -> Bool -> Bool -- 2 ^ 2 ^ 2 = 16
--fTwo :: Bool -> Quad -> Quad -- a -> b -> c = a -> ( b -> c ) = (c ^ b) ^ a = (4 ^ 4) ^ 2 = 65536
data Silly a b c d =
  MkSilly a
          b
          c
          d
  deriving (Show)

data EsResultFound a = EsResultFound
  { _version :: String
  , _source :: a
  } deriving (Eq, Show)

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert'
  :: Ord a
  => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving (Eq, Ord, Enum, Show)

f :: Weekday -> String
f Monday = "1 Miller Time"
f Tuesday = "2 Miller Time"
f Wednesday = "3 Miller Time"
f Thursday = "4 Miller Time"
f Friday = "5 Miller Time"
-- 11.18
-- 1 - a
-- 2 - c
-- 3 - b & c
-- 4 - c
--
