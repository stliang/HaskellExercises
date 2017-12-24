stop = "pbtdkg"

vowels = "aeiou"

nouns = "Steve Betty Abby Lin"

verbs = "walking standing"

a = [(x, y) | x <- stop, y <- vowels]

b = [(x, y) | x <- stop, y <- vowels, x == 'p']

c = [(x, y, z) | x <- words nouns, y <- words verbs, z <- words nouns]

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc'
  :: Fractional a
  => String -> a
seekritFunc' x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . map f

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' = ((foldr (||) False) .) . map

myElem
  :: Eq a
  => a -> [a] -> Bool
myElem x = myAny (\y -> x == y)

myElem'
  :: Eq a
  => a -> [a] -> Bool
myElem' x = any (\y -> x == y)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x : []

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : foldr (\x y -> f x : y) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) =
  case (f x) of
    True -> x : myFilter f xs
    False -> myFilter f xs

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy (\x y -> if x > y then GT else LT) [1,2,3,6,1,3,8,2,1]
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go xs x
  where
    go [] acc = acc
    go (y:[]) acc =
      case (f acc y) of
        GT -> acc
        _ -> y
    go (y:ys) acc =
      case (f acc y) of
        GT -> go ys acc
        _ -> go ys y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go xs x
  where
    go [] acc = acc
    go (y:[]) acc =
      case (f acc y) of
        LT -> acc
        _ -> y
    go (y:ys) acc =
      case (f acc y) of
        LT -> go ys acc
        _ -> go ys y
