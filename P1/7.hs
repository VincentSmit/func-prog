import FPPrac

r :: Number -> Number -> [Number]
r s v = [s] ++ r (s+v) v

r1 :: Number -> Number -> Number -> Number
r1 s v i = (r s v) FPPrac.!!  i

r2 :: Number -> Number -> Number -> Number -> Number
r2 s v i j = sum (FPPrac.drop i (FPPrac.take j (r s v)) )