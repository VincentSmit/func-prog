import FPPrac

saldo :: Number -> Number -> Number -> Number

saldo r b 0 = b

saldo r b n = (1 + r/100) * saldo r b (n-1)