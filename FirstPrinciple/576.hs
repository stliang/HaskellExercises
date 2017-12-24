import Data.List

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

takeFibs = (flip take) fibs

takeWhileFibs = takeWhile (\x -> x < 100) fibs

-- Factorial: 3! = 3 * 2 * 1
fac' x = foldl' (*) 1 [1 .. x]

fac x = go ys
  where
    ys = scanl (*) 1 [1 .. x]
    go (z:[]) = z
    go (z:zs) = go zs
