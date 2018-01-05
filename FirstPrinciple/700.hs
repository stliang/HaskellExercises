module Phone where

import Control.Exception.Base
import Data.Char
import Data.List

data DaPhone =
  DaPhone [[Char]]
  deriving (Show)

daPhone =
  DaPhone
    ["1", "2abc", "3def", "4ghi", "5jkl", "6mno", "7pqrs", "8tuv", "9wxyz", "*^", "0+ ", "#.,"]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok, Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

newtype Digit =
  Digit Char
  deriving (Show)

validDigits = "123456789*0#"

mkDigit :: Char -> Digit
mkDigit c = assert (c `elem` validDigits) Digit c

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) c = caseUnit ++ [(digit, (go' (keys !! index) 0))]
  where
    c' = toLower c
    caseUnit =
      if (isUpper c)
        then [(mkDigit '*', 1)]
        else []
    digit = mkDigit (validDigits !! index)
    index = go keys 0
    go [] acc = acc
    go (x:xs) acc =
      if (c' `elem` x)
        then acc
        else go xs (acc + 1)
    go' [] acc = acc
    go' (x:xs) acc =
      if (x == c')
        then acc
        else go' xs (acc + 1)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(Digit x, y) acc -> acc + y) 0

mostPopularLetter :: String -> Char
mostPopularLetter s = fst mostOcc
  where
    countByChar chars = fmap (\x -> (x, length $ filter (\y -> x == y) s)) chars
    countByKey (DaPhone keyChars) = fmap countByChar keyChars
    mostOcc =
      maximumBy
        (\(_, n) (_, n') ->
           if n > n'
             then GT
             else LT) $
      concat $ countByKey daPhone

coolestWord :: [[Char]] -> String
coolestWord xs = fst mostOcc
  where
    tokenWords :: [String]
    tokenWords = concat $ fmap words xs
    counts :: [(String, Int)]
    counts = nub $ fmap (\x -> (x, length $ filter (\y -> x == y) tokenWords)) tokenWords
    mostOcc =
      maximumBy
        (\(_, n) (_, n') ->
           if n > n'
             then GT
             else LT) $
      counts
