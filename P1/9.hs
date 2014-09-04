import FPPrac

rijlen :: [[Number]] -> Bool
rijlen [] =  True
rijlen (xs:xss) | FPPrac.length xss == 1 = True
                | (FPPrac.length xs)/=(FPPrac.length (head xss)) = False
                | otherwise = rijlen xss

rijsom :: [[Number]] -> [Number]
rijsom [] = []
rijsom (xs:xss) = [sum xs] ++ rijsom xss