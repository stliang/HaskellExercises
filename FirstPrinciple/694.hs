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
