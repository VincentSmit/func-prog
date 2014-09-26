import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char
import Debug.Trace

data BinTree a b = LeafBT b
                | NodeBT a (BinTree a b) (BinTree a b)
                deriving (Show, Eq)

data Cat = E | O | G | I

type TreeEx = BinTree Operator (Either Char Number)

data Operator = Plus | Minus | Div | Mult | Pow 
        deriving (Show, Eq)

data Delimiter = Open | Close 
        deriving (Show, Eq)

data Token =  TokNum Number 
            | TokOp Operator 
            | TokDel Delimiter
            | TokId Char
        deriving (Show, Eq)

tokToVal :: Token -> Either Char Number
tokToVal (TokNum v) = Right v
tokToVal (TokId v) = Left v

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
                | isDigit x = TokNum (read [x]) : tokenize xs
                | isAlpha x = TokId x : tokenize xs
                | isSpace x = tokenize xs
                | isDel x   = TokDel (delimiter x) : tokenize xs
                | otherwise = error "Unknown token."

parse2 :: Cat -> [Token] -> (TreeEx, [Token])
parse2 E (x:xs) | x == TokDel Open  = (NodeBT top t0 t2, tail r2)
                | otherwise = case tokToVal y of
                                Left y -> parse2 I (x:xs)
                                Right y -> parse2 G (x:xs)
            where
                (t0, r0) = parse2 E xs
                (NodeBT top _ _, r1) = parse2 O r0
                (t2, r2) = parse2 E r1
                y = x

parse2 O (x:xs) = (NodeBT (tokToOp x) (LeafBT (Right 0)) (LeafBT (Right 0)), xs)

parse2 G (x:xs) = (LeafBT (tokToVal x), xs)
parse2 I (x:xs) = (LeafBT (tokToVal x), xs)


eval :: String -> Number
eval expr = calc x
        where 
            (x,y) = (parse2 E (tokenize expr))

calc:: TreeEx-> Number
calc (NodeBT op t1 t2)  | op == Plus  = (calc t1) + (calc t2)
                        | op == Minus = (calc t1) - (calc t2)
                        | op == Mult  = (calc t1) * (calc t2)
                        | op == Div   = (calc t1) / (calc t2)
                        | op == Pow   = (calc t1) ^ (calc t2)

calc (LeafBT (Right num)) = num
calc (LeafBT (Left ident)) = assign ident

assign:: Char -> Number
assign c = FPPrac.ord c