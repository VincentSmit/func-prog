import FPPrac

allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ map  (== head xs) (tail xs)

isRR :: [Number] -> Bool
isRR xs = allEqual $ zipWith (-) (FPPrac.drop 1 xs) (FPPrac.take ((FPPrac.length xs ) -1) xs)