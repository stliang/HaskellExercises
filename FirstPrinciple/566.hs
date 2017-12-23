import Data.List
import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr
    (\x y ->
       case x of
         DbDate x' -> x' : y
         _ -> y)
    []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr
    (\x y ->
       case x of
         DbNumber x' -> x' : y
         _ -> y)
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  maximumBy
    (\x y ->
       case (x < y) of
         True -> LT
         _ -> GT) .
  filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral sum) / (fromIntegral n)
  where
    dbNumbers = filterDbNumber xs
    n = fromIntegral $ length dbNumbers
    sum = sumDb xs
