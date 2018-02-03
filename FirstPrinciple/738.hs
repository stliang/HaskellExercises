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

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs =
  if (Nothing `elem` xs)
    then Nothing
    else Just $ catMaybes xs
