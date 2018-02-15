import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)

half
  :: Fractional a
  => a -> a
half x = x / 2

halfIdentity
  :: Fractional c
  => c -> c
halfIdentity = (* 2) . half

prop_n = (\x -> halfIdentity x == (x :: Float))

listOrdered
  :: (Ord a)
  => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_listOrdered
  :: (Ord a)
  => [a] -> Bool
prop_listOrdered = listOrdered . sort

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck (prop_listOrdered :: [String] -> Bool)
{-
  hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ do digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
      it "returns [1] for 1" $ do digits 1 `shouldBe` [1]
      it "returns [1,0,0] for 100" $ do digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $ do wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    describe "halfIdentity" $ do
      it "halfIdentity of any number x equals x" $ do
        property $ \x -> halfIdentity x == (x :: Float)
    describe "sorted list property" $ do
      it "listOrdered of sorted list equals True" $ do
        property $ \x -> halfIdentity x == (x :: Float)
-}
