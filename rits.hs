rits :: [a] -> [b] -> [(a, b)]
rits xs [] = []
rits [] xs = []
rits (x:xs) (y:ys) = (x,y) : rits xs ys
