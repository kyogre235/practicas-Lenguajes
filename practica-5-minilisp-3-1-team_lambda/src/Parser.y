{
module Parser where

import Lexer

import Data.List
}

%name parser
%tokentype { Token }
%error { parseError }

{- Definición de los tokens -}

%token 

  num      { TokenNum $$ }
  bool     { TokenBool $$ }
  '+'      { TokenAdd }
  '-'      { TokenSub }
  '*'      { TokenMult }
  '/'      { TokenDiv }
  "add1"   { TokenAdd1 }
  "sub1"   { TokenSub1 }
  "sqrt"   { TokenSqrt }
  "expt"   { TokenExp }
  '<'      { TokenMq }
  '>'      { TokenM }
  '='      { TokenEq }
  "not"    { TokenNot }
  "or"     { TokenOr }
  "and"    { TokenAnd }
  '('      { TokenPA }
  ')'      { TokenPC }
  '['      { TokenPA1 }
  ']'      { TokenPC1 }
  id       { TokenId $$ }
  "let"    { TokenLet }
  "let*"   { TokenLet1 }
  "lambda" { TokenLambda }
  "if"     { TokenIf }
  "else"   { TokenElse }
  "cond"   { TokenCond }

%%

SASA : '(' Op SASAs ')'                                { $2 $3 }
    | '(' "let" '(' BDY ')' SASA  ')'                  { Let $4 $6 }
    | '(' "let*" '(' BDY ')' SASA ')'                  { Le1 $4 $6 }
    | '(' "if" SASA SASA SASA ')'                      { Ift $3 $4 $5 }
    | '(' "cond" '(' Conds ')' '(' "else" SASA ')' ')' { Con $4 $8 }
    | '(' "lambda" '(' Param ')' SASA ')'              { Lam $4 $6 }
    | '(' SASA SASAs ')'                               { App $2 $3 }
    | num                                              { Num $1 }
    | bool                                             { Boo $1 }
    | id                                               { Str $1 }

Conds : '[' SASA SASA ']'                    { [($2, $3)] }
      | '[' SASA SASA ']' Conds              { ($2, $3) : $5 }


Param : SASA                  { [$1] }   
      | SASA Param            { $1:$2 } 

BDY : '[' id SASA ']'         { [((Str $2),$3)] }
    | '[' id SASA ']' BDY     { ((Str $2),$3):$5 }

SASAs : SASA                  { [$1] }
     | SASA SASAs             { $1 : $2 }

Op : '+'     { Add }
   | '-'     { Sub }
   | '*'     { Mul }
   | '/'     { Div }
   | "add1"  { Ad1 }
   | "sub1"  { Su1 }
   | "sqrt"  { Sqt }
   | "expt"  { Exp }
   | '<'     { Meq }
   | '>'     { Maq }
   | '='     { Equ }
   | "not"   { Not }
   | "or"    { Orb }
   | "and"   { And }


{- Definición de la sintaxis concreta -}

{

parseError :: [Token] -> a
parseError x = error "Parse error"

data SASA = Num Double
          | Str String
          | Boo Bool
          | Add [SASA]
          | Sub [SASA]
          | Mul [SASA]
          | Div [SASA]
          | Ad1 [SASA]
          | Su1 [SASA]
          | Sqt [SASA]
          | Exp [SASA]
          | Maq [SASA]
          | Meq [SASA]
          | Equ [SASA]
          | Not [SASA]
          | And [SASA]
          | Orb [SASA]
          | Let [(SASA,SASA)] SASA
          | Le1 [(SASA,SASA)] SASA
          | Ift SASA SASA SASA
          | Con [(SASA,SASA)] SASA
          | Lam [SASA] SASA
          | App SASA [SASA]


instance Show SASA where
  show (Str s) = s 
  show (Num d) = show d
  show (Boo b) = if b then "#t" else "#f"
  show (Add xs) = "(" ++ "+ " ++ showListWithoutBrackets xs ++ ")"
  show (Sub xs) = "(" ++ "- " ++ showListWithoutBrackets xs ++ ")"
  show (Mul xs) = "(" ++ "* " ++ showListWithoutBrackets xs ++ ")"
  show (Div xs) = "(" ++ "/ " ++ showListWithoutBrackets xs ++ ")"
  show (Ad1 xs) = "(" ++ "add1 " ++ showListWithoutBrackets xs ++ ")"
  show (Su1 xs) = "(" ++ "sub1 " ++ showListWithoutBrackets xs ++ ")"
  show (Sqt xs) = "(" ++ "sqrt " ++ showListWithoutBrackets xs ++ ")"
  show (Exp xs) = "(" ++ "expt " ++ showListWithoutBrackets xs ++ ")"
  show (Maq xs) = "(" ++ "> " ++ showListWithoutBrackets xs ++ ")"
  show (Meq xs) = "(" ++ "< " ++ showListWithoutBrackets xs ++ ")"
  show (Equ xs) = "(" ++ "= " ++ showListWithoutBrackets xs ++ ")"
  show (Not xs) = "(" ++ "not " ++ showListWithoutBrackets xs ++ ")"
  show (And xs) = "(" ++ "and " ++ showListWithoutBrackets xs ++ ")"
  show (Orb xs) = "(" ++ "or " ++ showListWithoutBrackets xs ++ ")"
  show (Let xs ys) = "(" ++ "let " ++ "(" ++ showListWithoutBrackets2 xs ++ ") " ++ show ys ++ ")"
  show (Le1 xs ys) = "(" ++ "let* " ++ "(" ++ showListWithoutBrackets2 xs ++ ") " ++ show ys ++ ")"
  show (Lam xs ys) = "(" ++ "lambda " ++ "(" ++ showListWithoutBrackets xs ++ ") " ++ show ys ++ ")"
  show (Ift a b c) = "(" ++ "if "++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"
  show (Con xs ys) = "(" ++ "cond " ++ "(" ++ showListWithoutBrackets2 xs ++ ") " ++ "("++ "else " ++ show ys ++")"++ ")"
  show (App xs zs) = "(" ++ show xs ++ " " ++ showListWithoutBrackets zs ++ ")"

-- Función auxiliar para mostrar la lista sin corchetes ni comas
showListWithoutBrackets :: [SASA] -> String
showListWithoutBrackets = unwords . map show

showListWithoutBrackets2 :: [(SASA,SASA)] -> String
showListWithoutBrackets2 [] = ""
showListWithoutBrackets2 ((a,b):xs) = "[" ++ show a ++ " " ++ show b ++ "]" ++ showRest xs
  where
    showRest [] = ""
    showRest ys = " " ++ showListWithoutBrackets2 ys 


}
