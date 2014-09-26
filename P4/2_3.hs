import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char
import Debug.Trace

data BinTree a b = LeafBT b
                | NodeBT a (BinTree a b) (BinTree a b)
                deriving (Show, Eq)

data Cat = E | O | G | I

type TreeEx = BinTree Operator (Either String Number)

data Operator = Plus | Minus | Div | Mult | Pow 
        deriving (Show, Eq)

data Delimiter = Open | Close 
        deriving (Show, Eq)

data Token =  TokNum Number 
            | TokOp Operator 
            | TokDel Delimiter
            | TokId String
        deriving (Show, Eq)

tokToVal :: Token -> Either String Number
tokToVal (TokNum v) = Right v
tokToVal (TokId v) = Left v

tokToOp :: Token -> Operator
tokToOp (TokOp x) =  x

getNumber :: [Char] -> ([Char], [Char])
getNumber xs = (takeWhile (\x -> isDigit x) xs, dropWhile (\x -> isDigit x) xs)

getIdent :: [Char] -> ([Char], [Char])
getIdent xs = (takeWhile (\x -> isAlpha x) xs, dropWhile (\x -> isAlpha x) xs)

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
                | isDigit x = TokNum (read y) : tokenize ys
                | isAlpha x = TokId z : tokenize zs
                | isSpace x = tokenize xs
                | isDel x   = TokDel (delimiter x) : tokenize xs
                | otherwise = error "Unknown token."
            where
                (y,ys) = getNumber (x:xs)
                (z,zs) = getIdent (x:xs)

parse3 :: Cat -> [Token] -> (TreeEx, [Token])
parse3 E (x:xs) | x == TokDel Open  = (NodeBT top t0 t2, tail r2)
                | otherwise = case tokToVal y of
                                Left y -> parse3 I (x:xs)
                                Right y -> parse3 G (x:xs)
            where
                (t0, r0) = parse3 E xs
                (NodeBT top _ _, r1) = parse3 O r0
                (t2, r2) = parse3 E r1
                y = x

parse3 O (x:xs) = (NodeBT (tokToOp x) (LeafBT (Right 0)) (LeafBT (Right 0)), xs)

parse3 G (x:xs) = (LeafBT (tokToVal x), xs)
parse3 I (x:xs) = (LeafBT (tokToVal x), xs)


eval :: (String -> Number) -> String -> Number
eval func expr = calc func x
        where 
            (x,y) = (parse3 E (tokenize expr))

calc:: (String -> Number) -> TreeEx-> Number
calc func (NodeBT op t1 t2)  | op == Plus  = (calc func t1) + (calc func t2)
                        | op == Minus = (calc func t1) - (calc func t2)
                        | op == Mult  = (calc func t1) * (calc func t2)
                        | op == Div   = (calc func t1) / (calc func t2)
                        | op == Pow   = (calc func t1) ^ (calc func t2)

calc func (LeafBT (Right num)) = num
calc func (LeafBT (Left ident)) = func ident

assign:: String -> Number
assign c = sum $ map (\x -> FPPrac.ord x) (c)
