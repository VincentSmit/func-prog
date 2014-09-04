import FPPrac

codeer :: Number -> Char -> Char
codeer n l  | c>=65 && c<=90 = chr $(c + n - 65) `mod` 26 + 65
            | c>=97 && c<=122 = chr $(c + n - 97) `mod` 26 + 97
            | otherwise = chr c
            where
                c = ord l

-- map (codeer <number>) <string>