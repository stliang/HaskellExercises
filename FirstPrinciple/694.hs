import Data.Char

isSubseqOf
  :: (Eq a)
  => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf l@(x:xs) (y:ys) =
  case (x == y) of
    True -> isSubseqOf xs ys
    _ -> isSubseqOf l ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords inputString = zip old new
  where
    capHead = fmap (\(x:xs) -> toUpper x : xs)
    old = words inputString
    new = capHead old

capitalizeParagraph :: String -> String
capitalizeParagraph inputString = concat $ go (capitalizeWords inputString) True False
  where
    period :: [Char] -> Bool
    period [] = False
    period xs = (head $ reverse xs) == '.'
    go :: [(String, String)] -> Bool -> Bool -> [String]
    go [] _ _ = []
    go (x:[]) _ _ = [fst x]
    go (x:xs) fstChar aftPeriod =
      case fstChar || aftPeriod of
        True -> snd x : " " : go xs False (period (snd x))
        _ -> fst x : " " : go xs False (period (fst x))
