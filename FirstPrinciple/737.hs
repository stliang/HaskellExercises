data Nat
  = Zero
  | Succ Nat
  deriving (Show, Eq)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ go i Zero
  where
    go i' acc =
      if i' > 0
        then go (i' - 1) (Succ acc)
        else acc
