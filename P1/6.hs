import FPPrac

mylength :: [a] -> Number
mylength [] = 0
mylength(x:xs) = 1 + mylength xs

mysum :: [Number] -> Number
mysum [] = 0
mysum(x:xs) = x + mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse(x:xs) = (myreverse xs) ++ [x]

mytake :: [a] -> Number -> [a]
mytake [] n = []
mytake (x:xs) 0 = []
mytake (x:xs) n = [x] ++ mytake xs (n-1)

myelem :: [Number] -> Number -> String
myelem [] e = "Not found"
myelem (x:xs) e | x == e = "Element found"
                | otherwise = myelem xs e

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs)

mymaximum :: [Number] -> Number 
mymaximum [] = negate (1/0)
mymaximum (x:xs) | x > mymaximum xs = x
                  | x <= mymaximum xs = mymaximum xs

myzip :: [a] -> [b] -> [(a,b)]
myzip [] (y:ys) = []
myzip (x:xs) [] = []
myzip [] [] = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys