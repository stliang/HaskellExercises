isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: (a -> b) -> b -> Maybe a -> b
mayybee f _ (Just y) = f y
mayybee f x Nothing = x

fromMaybe :: a -> Maybe a -> a
fromMaybe = mayybee id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:ys) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = maybeToList x ++ (catMaybes xs)

flipMaybe
  :: (Eq a)
  => [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs =
  if (Nothing `elem` xs)
    then Nothing
    else Just $ catMaybes xs

leftToList :: Either a b -> [a]
leftToList (Left x) = [x]
leftToList (Right _) = []

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) = leftToList x ++ lefts' xs

lefts'' :: [Either a b] -> [a]
lefts'' = foldr (\x y -> leftToList x ++ y) []

rightToList :: Either a b -> [b]
rightToList (Left _) = []
rightToList (Right x) = [x]

right' :: [Either a b] -> [b]
right' = foldr (\x y -> rightToList x ++ y) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts, rights)
  where
    lefts = lefts'' xs
    rights = right' xs

eitherMaybe' :: (a -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left x) = Just $ f x
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' l _ (Left x) = l x
either' _ r (Right x) = r x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right x) = either' Just Just (Right $ f x)
