module Spec where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

data Trivial =
  Trivial
  deriving (Eq, Show)

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen
  :: Arbitrary a
  => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b =
  Pair a
       b
  deriving (Eq, Show)

pairGen
  :: (Arbitrary a, Arbitrary b)
  => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

sumGenEqual
  :: (Arbitrary a, Arbitrary b)
  => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

--instance Arbitrary a =>
--         Arbitrary (Maybe a) where
--  arbitrary = frequency [(1, return Nothing), (3, liftM Just arbitrary)]

sumGenFirstPls
  :: (Arbitrary a, Arbitrary b)
  => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain
