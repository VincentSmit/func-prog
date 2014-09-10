import FPPrac

isort :: [Number] -> [Number]
isort = foldr insert []

insert :: Number -> [Number] -> [Number]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

