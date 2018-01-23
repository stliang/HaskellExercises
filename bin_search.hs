module BinSearch where

--import Data.Array --(Array, Ix, (!), ListArray, Bounds)
import Data.Array

binsearch :: [Int] -> Int -> Int -> Int -> Int -- list, value, low, high, return int
binsearch xs value low high
  | high < low = -1
  | xs !! mid > value = binsearch xs value low (mid - 1)
  | xs !! mid < value = binsearch xs value (mid + 1) high
  | otherwise = mid
  where
    mid = low + ((high - low) `div` 2)

binsearch'
  :: (Ord a)
  => [a] -> a -> Int -> Int -> Maybe Int -- list, value, low, high, return int
binsearch' xs value low high
  | high < low = Nothing
  | pivot > value = binsearch' xs value low (mid - 1)
  | pivot < value = binsearch' xs value (mid + 1) high
  | otherwise = Just mid
  where
    mid = low + ((high - low) `div` 2)
    pivot = xs !! mid

--binarySearch'' haystack needle lo hi
--  | hi needle = binarySearch'' haystack needle lo (mid - 1)
--  | pivot < needle = binarySearch'' haystack needle (mid + 1) hi
--  | otherwise = Just mid
--  where
--    mid = lo + (hi - lo) `div` 2
--    pivot = haystack ! mid
-- Application to an array:
bSearchArray
  :: (Ix i, Integral i, Ord e)
  => Array i e -> e -> Maybe i
bSearchArray a x = bSearch (compare x . (a !)) (bounds a)

-- BINARY SEARCH --------------------------------------------------------------
bSearch
  :: Integral a
  => (a -> Ordering) -> (a, a) -> Maybe a
bSearch p (low, high)
  | high < low = Nothing
  | otherwise =
    let mid = (low + high) `div` 2
    in case p mid of
         LT -> bSearch p (low, mid - 1)
         GT -> bSearch p (mid + 1, high)
         EQ -> Just mid
