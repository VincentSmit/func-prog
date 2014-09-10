import FPPrac

ga :: (Number, Number, Number) -> Number
ga (a,_,_) = a

gb :: (Number, Number, Number) -> Number
gb (_,b,_) = b

gc :: (Number, Number, Number) -> Number
gc (_,_,c) = c

order :: [(Number, Number, Number)] -> [(Number, Number, Number)] 
order [] = []
order (x:xs) | (ga x) > (gb x) = (gb x, ga x, gc x) : order xs
             | otherwise = x : order xs 

removemult :: [(Number, Number, Number)] -> [(Number, Number, Number)]
removemult [] = []
removemult (x:xs) = x : removemult ( filter (\i -> (mod3 i x) /= 0) xs)

mod3 :: (Number, Number, Number) -> (Number, Number, Number) -> Number
mod3 (a,b,c) (x,y,z) = (a `mod` x) + (b `mod` y) + (c `mod` z)
 
pyth :: Number -> [(Number, Number, Number)]
pyth n = map (\(x,y) -> ((x^2-y^2), (2*x*y), (x^2+y^2))) ([(x,y) | x <- [1..n], y <- [1..n], x > y, (x^2+y^2)<=n])

pyth' :: Number -> [(Number, Number, Number)]
pyth' n = removemult (order (pyth n))
