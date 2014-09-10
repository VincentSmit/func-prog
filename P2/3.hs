import FPPrac 

-- a.
zeef :: [Number] -> [Number]
zeef (x:xs) = x : zeef (filter (\p -> (p `mod` x) /=0) (xs))

isPriem :: Number -> Bool
isPriem n = n == ( last $ takeWhile (\x -> x <= n) (zeef [2..]))

eersteN :: Number -> [Number]
eersteN n = FPPrac.take n (zeef [2..])

kleinerN :: Number -> [Number]
kleinerN n = takeWhile (\x -> x < n) (zeef [2..])

-- b.
delers :: Number -> [Number]
delers n = [ x | x <- [1..(n/2)] , n `mod` x == 0]