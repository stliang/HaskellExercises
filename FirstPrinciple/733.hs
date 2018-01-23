module P733 where

replaceThe :: String -> String
replaceThe s = concat $ fmap f tokens
  where
    tokens = words s
    f =
      (\x ->
         if (x == "the")
           then "a "
           else x ++ " ")

notThe :: String -> Maybe String
notThe s =
  if (s == "the")
    then Nothing
    else Just s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = count ws 0
  where
    ws = words s
    isVowel (x:xs) = x `elem` "aeiou"
    count :: [[Char]] -> Integer -> Integer
    count (x:[]) acc = acc
    count (x:y:zs) acc
      | x == "the" && (isVowel y) = count (y : zs) (acc + 1)
      | otherwise = count (y : zs) acc

countVowels :: String -> Int
countVowels s = length l
  where
    vowel = "aeiou"
    l = filter (\x -> x `elem` vowel) s

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s =
  if (vowelLength > consonantLength)
    then Nothing
    else Just $ Word' s
  where
    totalLength = length s
    consonantLength = totalLength - vowelLength
    vowelLength = countVowels s
