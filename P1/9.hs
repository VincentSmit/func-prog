import FPPrac

rijlen :: [[a]] -> Bool
rijlen [] =  True
rijlen [x] = True
rijlen (xs:xss) | (FPPrac.length xs)/=(FPPrac.length (head xss)) = False
                | otherwise = rijlen xss

rijsom :: [[Number]] -> [Number]
rijsom [] = []
rijsom (xs:xss) = [sum xs] ++ rijsom xss

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xss = (map head xss) : transpose (map tail xss)

kolsom :: [[Number]] -> [Number]
kolsom ([]:_) = []
kolsom (xss) = (sum (map head xss)) : kolsom (map tail xss