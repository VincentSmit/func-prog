import FPPrac
import Data.Char

-- a. (String, Number, Char, String)
data Persoon =  Persoon { naam :: String
                        , lt :: Number
                        , gs :: Char
                        , wp :: String
                        } deriving (Show)

-- b. Type Persoon gecreerd, functies komen gratis

-- c. Leeftijd drie maal
inclt_r :: [Persoon] -> Number -> [Persoon]
inclt_r [] _ = [] 
inclt_r (p:ps) n = (Persoon {naam = naam p, lt = (lt p + n), gs = gs p, wp = wp p}) : (inclt_r ps n)

inclt_l :: [Persoon] -> Number -> [Persoon]
inclt_l ps n = [Persoon {naam =naam p, lt = (lt p +n), gs = gs p, wp = wp p} | p <- ps ]

inclt_h :: [Persoon] -> Number -> [Persoon]
inclt_h ps n = map (\p -> Persoon{naam=naam p, lt=(lt p + n), gs=gs p, wp = wp p}) ps

-- d. 30<leeftijd<40
midage_r :: [Persoon] -> [Persoon]
midage_r [] = []
midage_r (p:ps) | (lt p > 30) && (lt p < 40) = p : midage_r ps
                | otherwise = midage_r ps

midage_l :: [Persoon] -> [Persoon]
midage_l ps = [p | p <- ps, lt p > 30, lt p < 40]

midage_h :: [Persoon] -> [Persoon]
midage_h ps = filter (\p -> lt p >30 && lt p < 40) ps

-- e. leeftijd door naam
getage :: [Persoon] -> String -> Number
getage ps s = lt  $ head $ filter (\p -> (map toLower (naam p)) == (map toLower s)) ps