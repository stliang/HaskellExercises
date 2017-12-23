myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

play :: [a] -> a -> [a]
play [] y = []
play (x:[]) y = [y]
play (x:xs) y = [x]

eft
  :: (Enum a, Ord a)
  => a -> a -> [a]
eft fstA lstA = go fstA []
  where
    go curA acc
      | curA < lstA =
        let sucA = succ curA
        in curA : (go sucA (acc ++ [sucA]))
      | otherwise = [lstA]

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

firstSen = "Tyger Tyger, buring bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy feaful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

tokenString :: Char -> [Char] -> [[Char]]
tokenString seperator inputString = go inputString []
  where
    f = dropWhile (== seperator) . dropWhile (/= seperator)
    go [] _ = []
    go s'@(x:xs) acc = go (f s') acc ++ [(takeWhile (/= seperator) s')]
