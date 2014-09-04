import FPPrac

allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ map  (== head xs) (tail xs)
