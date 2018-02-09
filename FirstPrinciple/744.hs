myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    (Just (y, z)) -> [y] ++ (myUnfoldr f z)
    _ -> []

myIterate' :: (a -> a) -> a -> [a]
myIterate' f x = myUnfoldr (\y -> Just (y, f y)) x

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = 
  case f x of
    (Just (l, m, r)) -> Node (unfold f l) m (unfold f r)
    _ -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n
  | n < 0 = Leaf
  | otherwise = unfold f 0
  where
    f k
      | k == n = Nothing
      | otherwise = Just (k + 1, k, k + 1)
