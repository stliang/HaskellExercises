-- (((1 * 1) * 2) * 3)
z = foldl (flip (*)) 1 [1 .. 3]

a = foldr (++) "" ["woot", "WOOT", "woot"]

b = foldr max 'a' "fear is the little death"

c = foldr (\x y -> and (x : y : [])) True [False, True]

d = foldr (||) True [False, True]

e = foldl (flip $ (++) . show) "" [1 .. 5]

f = foldr (flip const) 'a' [1 .. 5]

g = foldr (flip const) 0 "tacos"

h = foldl const 0 "burritos"

i = foldl const 'z' [1 .. 5]
