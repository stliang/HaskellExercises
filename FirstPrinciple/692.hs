module Cipher where

import Data.Char

shiftRight :: Int -> Char -> Char
shiftRight 0 x = x
shiftRight n x =
  case x of
    'z' -> go 'a'
    'Z' -> go 'A'
    _ -> go (succ x)
  where
    go = shiftRight (n - 1)

shiftLeft :: Int -> Char -> Char
shiftLeft 0 x = x
shiftLeft n x =
  case x of
    'a' -> go 'z'
    'A' -> go 'Z'
    _ -> go (pred x)
  where
    go = shiftLeft (n - 1)

cipher :: Int -> [Char] -> [Char]
cipher _ [] = []
cipher n (x:xs) = shiftRight n x : cipher n xs

uncipher :: Int -> [Char] -> [Char]
uncipher _ [] = []
uncipher n (x:xs) = shiftLeft n x : uncipher n xs

-- Assume all input characters are in Cap
-- ord 'A' = 65
-- Skip space
-- TODO: Fix this
vcipher :: [Char] -> [Char] -> [Char]
vcipher keyword phrase = go keyword phrase []
  where
    go _ [] acc = acc
    go [] ys acc = go keyword ys acc
    go (x:xs) (y:ys) acc = go xs ys $ (shiftRight (ord x - 65) y) : acc

upperCase :: [Char] -> [Char]
upperCase = filter isUpper

capFirstChar :: [Char] -> [Char]
capFirstChar [] = []
capFirstChar (x:xs) = toUpper x : xs

capChar :: [Char] -> [Char]
capChar [] = []
capChar (x:xs) = toUpper x : capChar xs

headCap :: [Char] -> [Char]
headCap [] = []
headCap (x:_) = toUpper x : []

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) =
  case x of
    True -> myOr xs
    False -> False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  case (f x) of
    True -> True
    False -> myAny f xs

myElem
  :: Eq a
  => a -> [a] -> Bool
myElem x xs = myAny (\y -> y == x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = (squishMap (\y -> [y]) x) ++ squishAgain xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go x xs x
  where
    go _ [] acc = acc
    go x (y:[]) acc =
      case (f x y) of
        GT ->
          case (f x acc) of
            GT -> x
            _ -> acc
        _ ->
          case (f y acc) of
            GT -> y
            _ -> acc
    go x (y:ys) acc =
      case (f x y) of
        GT ->
          case (f x acc) of
            GT -> go y ys x
            _ -> go y ys acc
        _ ->
          case (f y acc) of
            GT -> go y ys y
            _ -> go y ys acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = myMaximumBy (flip f) xs

myMaximum
  :: (Ord a)
  => [a] -> a
myMaximum xs =
  myMaximumBy
    (\x y ->
       if x > y
         then GT
         else LT)
    xs

myMinimum
  :: (Ord a)
  => [a] -> a
myMinimum xs =
  myMinimumBy
    (\x y ->
       if x > y
         then GT
         else LT)
    xs
