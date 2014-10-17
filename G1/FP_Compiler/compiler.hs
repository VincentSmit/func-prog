--   _                                _   _             _                                                                      _ _              --
--  (_)_ __ ___  _ __   ___ _ __ __ _| |_(_)_   _____  | | __ _ _ __   __ _ _   _  __ _  __ _  ___    ___ ___  _ __ ___  _ __ (_) | ___ _ __    --
--  | | '_ ` _ \| '_ \ / _ \ '__/ _` | __| \ \ / / _ \ | |/ _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \  / __/ _ \| '_ ` _ \| '_ \| | |/ _ \ '__|   --
--  | | | | | | | |_) |  __/ | | (_| | |_| |\ V /  __/ | | (_| | | | | (_| | |_| | (_| | (_| |  __/ | (_| (_) | | | | | | |_) | | |  __/ |      --
--  |_|_| |_| |_| .__/ \___|_|  \__,_|\__|_| \_/ \___| |_|\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___|  \___\___/|_| |_| |_| .__/|_|_|\___|_|      --
--              |_|                                                   |___/             |___/                           |_|                     --
--                                                                                                                                              --

import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char

--                                        _                 --
--   _ __   __ _ _ __ __ _ _ __ ___   ___| |_ ___ _ __ ___  --
--  | '_ \ / _` | '__/ _` | '_ ` _ \ / _ \ __/ _ \ '__/ __| --
--  | |_) | (_| | | | (_| | | | | | |  __/ ||  __/ |  \__ \ --
--  | .__/ \__,_|_|  \__,_|_| |_| |_|\___|\__\___|_|  |___/ --
--  |_|                                                     --
--                                                          --



--       _       _              _                   _                           --
--    __| | __ _| |_ __ _   ___| |_ _ __ _   _  ___| |_ _   _ _ __ ___  ___     --
--   / _` |/ _` | __/ _` | / __| __| '__| | | |/ __| __| | | | '__/ _ \/ __|    --
--  | (_| | (_| | || (_| | \__ \ |_| |  | |_| | (__| |_| |_| | | |  __/\__ \    --
--   \__,_|\__,_|\__\__,_| |___/\__|_|   \__,_|\___|\__|\__,_|_|  \___||___/    --
--                                                                              --

-- Arithmetic Operators
data AOperator = PLUS | MINUS | DIV | MULT | MOD | NaAOp
        deriving (Show, Eq)

-- Relation Operators
data ROperator =  ASSIGN | LESS | GREAT | LESSEQ | GREATEQ | EQUAL | NOTEQUAL | NaROp
        deriving (Show, Eq)

-- Boolean Operators
data BOperator = AND | OR | NOT | NaBOp
        deriving (Show, Eq)

data Delimiter = RPAREN | LPAREN | RCURLY | LCURLY | SEMI | NaD
        deriving (Show, Eq)

data Keyword = IF | THEN | ELSE | WHILE | DO | FOR | FALSE | TRUE | NaK
        deriving (Show, Eq)

type Program    = [Statement]

data BExpression= BoolConst Bool
                | Not BExpression
                | BBinary BOperator BExpression BExpression
                | RBinary ROperator AExpression AExpression
        deriving (Show)

data AExpression= Var String
                | IntConst Integer
                | ABinary AOperator AExpression AExpression
        deriving (Show)

data Statement  = Assign String AExpression
                | If BExpression Statement Statement
                | While BExpression Statement
                | Sequence [Statement]
        deriving (Show)

--   _        _              _                  --
--  | |_ ___ | | _____ _ __ (_)_______ _ __     --
--  | __/ _ \| |/ / _ \ '_ \| |_  / _ \ '__|    --
--  | || (_) |   <  __/ | | | |/ /  __/ |       --
--   \__\___/|_|\_\___|_| |_|_/___\___|_|       --
--                                              --

data Token =  TokNum Integer 
            | TokAOp AOperator 
            | TokBOp BOperator
            | TokROp ROperator
            | TokDel Delimiter
            | TokKey Keyword
            | TokId String
            | TokEnd
        deriving (Show, Eq)

getNumber :: [Char] -> ([Char], [Char])
getNumber xs = (takeWhile (\x -> isDigit x) xs, dropWhile (\x -> isDigit x) xs)

getIdent :: [Char] -> ([Char], [Char])
getIdent xs = (takeWhile (\x -> isAlpha x) xs, dropWhile (\x -> isAlpha x) xs)

getOperator :: [Char] -> ([Char], [Char])
getOperator (x:x1:xs)   | isOp x1   = (([x]++[x1]), xs) 
                        | otherwise = ([x], (x1:xs))

aOperator :: String -> AOperator
aOperator c | c == "+"  = PLUS
            | c == "-"  = MINUS
            | c == "/"  = DIV
            | c == "*"  = MULT
            | c == "%"  = MOD
            | otherwise = error "Invalid operator."

rOperator :: String -> ROperator
rOperator c | c == "="  = ASSIGN
            | c == "<"  = LESS
            | c == ">"  = GREAT
            | c == "<=" = LESSEQ
            | c == ">=" = GREATEQ
            | c == "==" = EQUAL
            | c == "<>" = NOTEQUAL
            | otherwise = error "Invalid operator."

bOperator :: String -> BOperator
bOperator c | c == "&&" = AND
            | c == "||" = OR
            | c == "!"  = NOT       
            | otherwise = error "Invalid operator."

delimiter :: Char -> Delimiter
delimiter c | c == '('  = LPAREN
            | c == ')'  = RPAREN
            | c == '{'  = LCURLY
            | c == '}'  = RCURLY
            | c == ';'  = SEMI

keyword :: String -> Keyword
keyword s   | s == "if"     = IF
            | s == "then"   = THEN
            | s == "else"   = ELSE
            | s == "while"  = WHILE
            | s == "do"     = DO

isDel :: Char -> Bool
isDel c = elem c "(){};"

isOp :: Char -> Bool
isOp c = isAOp c || isBOp c || isROp c
 
isAOp :: Char -> Bool
isAOp c = elem c "+-*/^%"

isBOp :: Char -> Bool
isBOp c = elem c "&|!"

isROp :: Char -> Bool
isROp c = elem c "=<>"

isBOpToken :: Token -> Bool
isBOpToken t = elem t [TokBOp OR, TokBOp AND, TokBOp NOT]

isROpToken :: Token -> Bool
isROpToken t = elem t [TokROp LESS,TokROp GREAT,TokROp LESSEQ,TokROp GREATEQ,TokROp EQUAL,TokROp NOTEQUAL]

isAOpToken :: Token -> Bool
isAOpToken t = elem t [TokAOp PLUS,TokAOp MINUS,TokAOp DIV,TokAOp MULT,TokAOp MOD]

isOpToken :: Token -> Bool
isOpToken t = isROpToken t || isAOpToken t || isBOpToken t

isNumToken :: Token -> Bool
isNumToken (TokNum x) = True
isNumToken _ = False 

isIdToken :: Token -> Bool
isIdToken (TokId x) = True
isIdToken _ = False 

isKey :: String -> Bool
isKey s = elem s ["if","then","else","while","do","where"]

isBoolConst :: String -> Bool
isBoolConst s = elem s ["false", "true"]

tokToDel :: Token -> Delimiter
tokToDel (TokDel t) = t
tokToDel _ = NaD

tokToAOp :: Token -> AOperator
tokToAOp (TokAOp t) = t
tokToAOp _ = NaAOp

tokToBOp :: Token -> BOperator
tokToBOp (TokBOp t) = t
tokToBOp _ = NaBOp

tokToROp :: Token -> ROperator
tokToROp (TokROp t) = t
tokToROp _ = NaROp

tokToKey :: Token -> Keyword
tokToKey (TokKey t) = t
tokToKey _ = NaK

tokToId :: Token -> String
tokToId (TokId t) = t
tokToId _ = "?"

tokToInt :: Token -> Integer
tokToInt (TokNum t) = t
tokToInt _ = -1

tokenize :: String -> [Token]
tokenize [] = [TokEnd]
tokenize (x:xs) | isAOp x   = TokAOp (aOperator o) : tokenize os
                | isROp x   = TokROp (rOperator o) : tokenize os
                | isBOp x   = TokBOp (bOperator o) : tokenize os
                | isDigit x = TokNum (read y) : tokenize ys
                | isAlpha x = (if (isKey z) || (isBoolConst z) then TokKey (keyword z) else TokId z) : tokenize zs
                | isSpace x = tokenize xs
                | isDel x   = TokDel (delimiter x) : tokenize xs
                | otherwise = error "Unknown token."
            where
                (y,ys) = getNumber (x:xs)
                (z,zs) = getIdent (x:xs)
                (o,os) = getOperator (x:xs)

--   _ __   __ _ _ __ ___  ___ _ __     --
--  | '_ \ / _` | '__/ __|/ _ \ '__|    --
--  | |_) | (_| | |  \__ \  __/ |       --
--  | .__/ \__,_|_|  |___/\___|_|       --
--  |_|                                 --
--                                      --

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (x:xs) = x

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (x:xs) = xs

acceptToken :: Token -> [Token] -> [Token]
acceptToken t (x:xs) = if t == x then xs else error "Unexpected token."

acceptAOpToken :: [Token] -> (Token, [Token])
acceptAOpToken ts = if isAOpToken (lookAhead ts)
                        then (lookAhead ts,(accept ts))
                        else error "Invalid operator"

acceptBOpToken :: [Token] -> (Token, [Token])
acceptBOpToken ts = if isBOpToken (lookAhead ts)
                        then (lookAhead ts,(accept ts))
                        else error "Invalid operator"

acceptROpToken :: [Token] -> (Token, [Token])
acceptROpToken ts = if isROpToken (lookAhead ts) 
                        then (lookAhead ts,(accept ts))
                        else error "Invalid operator"

parse :: [Token] -> Program
parse [TokEnd] = []
parse ts = s : (parse ts2)
            where
                (s, ts2) = parseStatement ts

parseStatement :: [Token] -> (Statement, [Token])
parseStatement (t:ts)   | tokToKey t == IF      = parseIf (t:ts)
                        | tokToKey t == WHILE   = parseWhile (t:ts)
                        | isIdToken t           = parseAssign (t:ts)
                        | otherwise             = error "No statement found"

parseIf :: [Token] -> (Statement, [Token])
parseIf ts = (If e s1 s2, ts7)
    where  
        ts1 = acceptToken (TokKey IF) ts
        (e, ts2) = parseBExpression ts1
        ts3 = acceptToken (TokKey THEN) ts2
        ts4 = acceptToken (TokDel LCURLY) ts3
        (s1, ts5) = parseStatement ts4
        ts6 = acceptToken (TokDel RCURLY) ts5
        (s2, ts7) = if lookAhead ts6 == TokKey ELSE then parseElse ts6 else (Sequence [], ts6)

parseElse :: [Token] -> (Statement, [Token])
parseElse ts = (s, ts4)
    where
        ts1 = acceptToken (TokKey ELSE) ts
        ts2 = acceptToken (TokDel LCURLY) ts1
        (s,ts3) = parseStatement ts2
        ts4 = acceptToken (TokDel RCURLY) ts3

parseWhile :: [Token] -> (Statement, [Token])
parseWhile ts = (While e s1, ts6)
    where  
        ts1 = acceptToken (TokKey WHILE) ts
        (e, ts2) = parseBExpression ts1
        ts3 = acceptToken (TokKey DO) ts2
        ts4 = acceptToken (TokDel LCURLY) ts3
        (s1, ts5) = parseStatement ts4
        ts6 = acceptToken (TokDel RCURLY) ts5

parseAssign :: [Token] -> (Statement, [Token])
parseAssign ts = (Assign s e, ts4)
    where
        (s, ts1) = parseVar ts
        ts2 = acceptToken (TokROp ASSIGN) ts1
        (e,ts3) = parseAExpression ts2
        ts4 = acceptToken (TokDel SEMI) ts3

parseBExpression :: [Token] -> (BExpression, [Token])
parseBExpression ts | lookAhead ts == (TokKey TRUE) || lookAhead ts == (TokKey FALSE) = parseBoolConst ts
                    | lookAhead ts == (TokBOp NOT) = parseNot ts
                    | lookAhead ts == (TokDel LPAREN) = if isROpToken (parseOperator ts' (-1))
                                                        then parseRBinary ts 
                                                        else parseBBinary ts
                    | otherwise = error "Invalid expression"
                where
                    ts' = ts

parseBoolConst :: [Token] -> (BExpression, [Token])
parseBoolConst ts = (BoolConst (lookAhead ts == TokKey TRUE), (accept ts))


parseNot :: [Token] -> (BExpression, [Token])
parseNot ts = (Not e, ts2)
    where  
        ts1 = acceptToken (TokBOp NOT) ts
        (e, ts2) = parseBExpression ts1

parseBBinary :: [Token] -> (BExpression, [Token])
parseBBinary ts = (BBinary (tokToBOp o) e1 e2, ts5)
    where  
        ts1 = acceptToken (TokDel LPAREN) ts
        (e1, ts2) = parseBExpression ts1
        (o, ts3) = acceptBOpToken ts2
        (e2, ts4) = parseBExpression ts3
        ts5 = acceptToken (TokDel RPAREN) ts4

parseRBinary :: [Token] -> (BExpression, [Token])
parseRBinary ts = (RBinary (tokToROp o) e1 e2, ts5)
    where
        ts1 = acceptToken (TokDel LPAREN) ts
        (e1, ts2) = parseAExpression ts1
        (o, ts3) = acceptROpToken ts2
        (e2, ts4) = parseAExpression ts3
        ts5 = acceptToken (TokDel RPAREN) ts4

parseOperator :: [Token] -> Integer -> Token
parseOperator ts i  | lookAhead ts == (TokDel LPAREN) = parseOperator (accept ts) (i+1)
                    | lookAhead ts == (TokDel RPAREN) = parseOperator (accept ts) (i-1)
                    | lookAhead ts == TokEnd = error "Operator not found."
                    | otherwise = if i == 0 && isOpToken (lookAhead ts)
                                    then head ts 
                                    else parseOperator (accept ts) i
                    

parseAExpression :: [Token] -> (AExpression, [Token])
parseAExpression ts | isIdToken (lookAhead ts) = (Var s, ts1)
                    | isNumToken (lookAhead ts) = (IntConst n, ts2)
                    | lookAhead ts == (TokDel LPAREN) = parseABinary ts
                    | otherwise = error "Invalid expression."
                where 
                    (s, ts1) = parseVar ts
                    (n, ts2) = parseInt ts

parseInt :: [Token] -> (Integer, [Token])
parseInt (t:ts) = ((tokToInt t), ts)

parseVar :: [Token] -> (String, [Token])
parseVar (t:ts) = ((tokToId t), ts) 

parseABinary :: [Token] -> (AExpression, [Token])
parseABinary ts = (ABinary (tokToAOp o) e1 e2, ts5)
    where  
        ts1 = acceptToken (TokDel LPAREN) ts
        (e1, ts2) = parseAExpression ts1
        (o, ts3) = acceptAOpToken ts2
        (e2, ts4) = parseAExpression ts3
        ts5 = acceptToken (TokDel RPAREN) ts4

--                                   _              --
--    __ _  ___ _ __   ___ _ __ __ _| |_ ___  _ __  --
--   / _` |/ _ \ '_ \ / _ \ '__/ _` | __/ _ \| '__| --
--  | (_| |  __/ | | |  __/ | | (_| | || (_) | |    --
--   \__, |\___|_| |_|\___|_|  \__,_|\__\___/|_|    --
--   |___/                                          --
--                                                  --

{-
-- Register stuff not working yet. Have to find out to memorize what registers are unused and what line in the program we're at.


compileP (Assign st ex) = [Load (compileE ex) r1]   -- r1 has to be first free register

compileP (If ex st1 st2) = [(compileP ex)] ++ [Jump CA p1] ++ [compileP st1] ++ [Jump UA p2 ]++ [compileP st2] -- p1,p2 are  behind If-statement and second statement
compileP (While ex st) = [compileP ex] ++ [Jump CA p1] ++ [compileP st] ++ [Jump UA p2] -- p1 = after while-statement, p2 = towards statement inside
compileP (Sequence (s:ss)) = (compileP s) : [compileP (Sequence ss)]

compileE (BoolConst b) = b
compileE (Not b) = Compute Not (compileE b) _ r1 -- r1 has to be first free register
compileE (BBinary op ex1 ex2) = Compute (compileE op) (compileE ex1) (compileE ex2) r1 -- r1 has to be first free register
compileE (RBinary LESSEQ ex1 ex2) = Compute lt (compileE ex1) ((compileE ex2)+1) r1 -- r1 has to be first free register
compileE (RBinary GREATEQ ex1 ex2) = Compute gt (compileE ex1) ((compileE ex2)-1) r1 -- r1 has to be first free register
compileE (RBinary op ex1 ex2) = Compute (compileE op) (compileE ex1) (compileE ex2) r1 -- r1 has to be first free register

compileE (Var s) = s -- s has to be the register where s is loaded
compileE (IntConst i) = i
compileE (ABinary op ex1 ex2) = Compute (compile op) (compileE ex1) (compileE ex2) r1 -- r1 has to be first free register

compileE PLUS = Add
compileE MINUS = Min
compileE DIV = Div
compileE MULT = Mul
compileE POW = Pow
compileE MOD = Mod

compileE GREAT = gt
compileE EQUAL = Equal
compileE NOTEQUAL = NEq
compileE LESS = lt

 
compileE AND = And
compileE OR = Or
compileE NOT = Not

--Compiles a free statement (the first) and adds an endprogram at the end
compileP s = (compileP s)++ [EndProg]
        
        

-}