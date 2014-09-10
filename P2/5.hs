import FPPrac

stijgend :: [Number] -> Bool
stijgend [] = True
stijgend [x] = True
stijgend (x:xs) | x >= head xs = False
                | otherwise = stijgend xs

avg :: [Number] -> Number
avg xs = (foldl (+) 0 xs)/ (FPPrac.length xs)

zwakStijgend :: [Number] -> Bool
zwakStijgend [] = True
zwakStijgend [x] = True
zwakStijgend xs | (last xs) < avg (init xs) = False
                | otherwise = zwakStijgend (init xs)