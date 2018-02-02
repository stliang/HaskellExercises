isPrime :: Integer -> Bool
isPrime 0 = True
isPrime 1 = True
isPrime i = b
  where
    xs = [2 .. (i - 1)]
    b = all (\x -> (i `mod` x) /= 0) xs
