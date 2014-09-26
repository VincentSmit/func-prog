import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char
import Debug.Trace

data BinTree a b = LeafBT b
                | NodeBT a (BinTree a b) (BinTree a b)
                deriving (Show, Eq)

data Cat = E | O | G

type TreeEx = BinTree Operator Number


data Operator = Plus | Minus | Div | Mult | Pow 
        deriving (Show, Eq)

data Delimiter = Open | Close 
        deriving (Show, Eq)

data Token =  TokNum Number 
            | TokOp Operator 
            | TokDel Delimiter
        deriving (Show, Eq)

toNum :: Char -> Number
toNum c = (FPPrac.ord c) - 48

tokToNum :: Token -> Number
tokToNum (TokNum x) = x

tokToOp :: Token -> Operator
tokToOp (TokOp x) =  x

operator :: Char -> Operator
operator c  | c == '+'  = Plus
            | c == '-'  = Minus
            | c == '/'  = Div
            | c == '*'  = Mult
            | c == '^'  = Pow

delimiter :: Char -> Delimiter
delimiter c | c == '('  = Open
            | c == ')'  = Close

isDel :: Char -> Bool
isDel c = elem c "()"
            
isOp :: Char -> Bool
isOp c = elem c "+-*/^"

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs) | isOp x    = TokOp (operator x) : tokenize xs
                | isDigit x = TokNum (toNum x) : tokenize xs
                | isSpace x = tokenize xs
                | isDel x   = TokDel (delimiter x) : tokenize xs
                | otherwise = error "Unknown token."

parse1 :: Cat -> [Token] -> (TreeEx, [Token])
parse1 E (x:xs) | x == TokDel Open  = (NodeBT top t0 t2, tail r2)
                | otherwise = parse1 G (x:xs)
            where
                (t0, r0) = parse1 E xs
                (NodeBT top _ _, r1) = parse1 O r0
                (t2, r2) = parse1 E r1

parse1 O (x:xs) = (NodeBT (tokToOp x) (LeafBT 0) (LeafBT 0), xs)

parse1 G (x:xs) = (LeafBT (tokToNum x), xs)

eval :: String -> Number
eval expr = calc x
        where (x,y) = (parse1 E (tokenize expr))

calc:: TreeEx-> Number
calc (NodeBT op t1 t2)  | op == Plus  = (calc t1) + (calc t2)
                        | op == Minus = (calc t1) - (calc t2)
                        | op == Mult  = (calc t1) * (calc t2)
                        | op == Div   = (calc t1) / (calc t2)
                        | op == Pow   = (calc t1) ^ (calc t2)

calc (LeafBT num) =  num