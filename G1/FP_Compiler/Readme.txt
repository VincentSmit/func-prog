Readme
run compiler.hs met ghci

belangrijke functies voor aanroepen:

tokenize :: String -> [Token]

parse :: [Token] -> Program

foutmelding van parser is primitief.

======================================================================
Grammatica

Program ::= Statement+;

Statement ::=	  Identifier '=' expression ';'
		| "if" Expression "then" '{' Statement '}' "else" '{' Statement '}'
		| "while" Expression "do" '{' Statement '}'

Expression ::= 	  Identifier
		| Integer
		| BoolConst
		| '(' Expression OPERATOR Expression ')'
			
Identifier ::= LETTER | LETTER LETTER

Integer ::= NUMBER | NUMBER NUMBER

BoolConst ::= "true" | "false"

OPERATOR = "+"|"-"|"/"|"*"|"%"| "||" | "&&" | "!" | "<" | ">" | "=" | "<>" | "<=" | ">=" | "=="

NUMBER ::= 1..9

LETTER ::= a..z | A..Z
