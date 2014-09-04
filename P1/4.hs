import FPPrac

discr :: Number -> Number -> Number -> Number
discr a b c = b^2-4*a*c

wortel1 :: Number -> Number -> Number -> Number 
wortel1 a b c   | d<0       = error "Discriminant negatief"
                | otherwise = ((b*(-1)) + sqrt d)/(2*a)
                where 
                    d = discr a b c

wortel2 :: Number -> Number -> Number -> Number
wortel2 a b c   | d<0       = error "Discriminant negatief"
                | otherwise = ((b*(-1)) - sqrt d)/(2*a)
                where
                    d = discr a b c