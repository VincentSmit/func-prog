import FPPrac

top :: Number -> Number -> Number -> (Number,Number)
top a b c = (extrX a b, extrY a b c)

extrX :: Number -> Number -> Number
extrX a b = (negate b)/(2*a)

extrY :: Number -> Number -> Number -> Number
extrY a b c = a*x^2+b*x+c
            where 
                x = extrX a b