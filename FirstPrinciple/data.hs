bSearch
  :: (Ord a)
  => a -- ^ the item to search for
  -> [a] -- ^ list of element to search
  -> Int -- ^ beginning of search window
  -> Int -- ^ end of search window
  -> Maybe Int -- ^ index of element
bSearch x xs low high
  | low > high = Nothing
  | pivot < x = bSearch x xs (mid + 1) high
  | pivot > x = bSearch x xs low (mid - 1)
  | otherwise = Just mid
  where
    mid = low + (high - low) `div` 2
    pivot = xs !! mid
