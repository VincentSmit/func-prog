import FPPrac
import Data.List
import Data.Function

split :: [a] -> Number -> [[a]]
split [] _ = []
split (xs) n | FPPrac.length xs == n = [xs]
             | otherwise = (FPPrac.take n xs) : (split (tail xs) n)


deellijst :: [Number] -> [Number] -> Bool
deellijst xs ys = or $ map (\y -> (sort xs) == (sort y)) (split ys (FPPrac.length xs))

sublijst :: (Eq a) => [a] -> [a] -> Bool
sublijst xs ys = elem xs (split ys (FPPrac.length xs))