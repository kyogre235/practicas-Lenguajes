module Lexer where

import Data.Char

data Token = TokenNum Double
            | TokenBool Bool
            | TokenId String
            | TokenAdd
            | TokenSub
            | TokenMult
            | TokenDiv
            | TokenAdd1
            | TokenSub1
            | TokenSqrt
            | TokenExp
            | TokenM
            | TokenEq
            | TokenMq
            | TokenNot
            | TokenAnd
            | TokenOr
            | TokenPA
            | TokenPC
            | TokenLet
            | TokenLet1
            | TokenLambda
            | TokenPA1
            | TokenPC1
            | TokenIf
            | TokenElse
            | TokenCond
           
instance Show Token where
  show :: Token -> String 
  show (TokenNum d) = show d 
  show (TokenBool b) = if b then "#t" else "#f"
  show (TokenId d) = d 
  show TokenAdd = "+"
  show TokenSub = "-"
  show TokenAdd1 = "add1"
  show TokenSub1 = "sub1"
  show TokenMult = "*"
  show TokenDiv = "/"
  show TokenExp = "expt"
  show TokenEq = "="
  show TokenM = ">"
  show TokenMq = "<"
  show TokenSqrt = "sqrt"
  show TokenNot = "not"
  show TokenAnd = "and"
  show TokenOr = "or"
  show TokenPA = "("
  show TokenPC = ")"
  show TokenPA1 = "["
  show TokenPC1 = "]"
  show TokenLet = "let"
  show TokenLet1 = "let*"
  show TokenLambda  = "lambda"
  show TokenIf = "if"
  show TokenElse = "else"
  show TokenCond = "cond"


doubleSpan :: String -> (String, String)
doubleSpan xs = 
    if null siguientesDigitos then 
        (signo ++ primerosDigitos, resto1)
    else 
        (signo ++ primerosDigitos ++ siguientesDigitos ++ epsilon, resto3)
    where
        -- Maneja el signo al comienzo (puede ser '-' o no)
        (signo, restoSigno) = case xs of
                                ('-':ys) -> ("-", ys)  -- Si empieza con '-', guarda el signo y el resto
                                _        -> ("", xs)   -- Si no, no hay signo y tomamos toda la cadena original

        -- Extrae los dígitos iniciales después del signo
        (primerosDigitos, resto1) = span isDigit restoSigno

        -- Maneja la parte decimal después del punto '.'
        (siguientesDigitos, resto2) = case resto1 of
                                        ('.':ys) -> let (ds, r) = span isDigit ys in ('.':ds, r)
                                        _        -> ([], resto1)

        -- Maneja la parte exponencial después de 'e' o 'e-'
        (epsilon, resto3) = case resto2 of
                                ('e':'-':ys) -> let (ds, r) = span isDigit ys in ('e':'-':ds, r)
                                ('e':ys)     -> let (ds, r) = span isDigit ys in ('e':ds, r)
                                _            -> ([], resto2)

varSpan :: String -> (String, String)
varSpan xs = span (\x -> isAlpha x || isDigit x || (x == '-') || (x == '?') || (x == '!') ) xs



lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs 
lexer ('(':xs) = TokenPA : lexer xs
lexer (')':xs) = TokenPC : lexer xs
lexer ('[':xs) = TokenPA1 : lexer xs
lexer (']':xs) = TokenPC1 : lexer xs 
lexer ('a':'d':'d':'1':xs) = TokenAdd1 : lexer xs
lexer ('s':'u':'b':'1':xs) = TokenSub1 : lexer xs
lexer ('+':xs) = TokenAdd : lexer xs
lexer ('*':xs) = TokenMult : lexer xs
lexer ('/':xs) = TokenDiv : lexer xs
lexer ('<':xs) = TokenMq : lexer xs
lexer ('>':xs) = TokenM : lexer xs
lexer ('=':xs) = TokenEq : lexer xs
lexer ('s':'q':'r':'t':xs) = TokenSqrt : lexer xs
lexer ('e':'x':'p':'t':xs) = TokenExp : lexer xs
lexer ('#':'t':xs) = TokenBool True : lexer xs
lexer ('#':'f':xs) = TokenBool False: lexer xs
lexer ('a':'n':'d':xs) = TokenAnd : lexer xs
lexer ('o':'r':xs) = TokenOr : lexer xs
lexer ('n':'o':'t':xs) = TokenNot : lexer xs
lexer ('l':'e':'t':'*':xs) = TokenLet1 : lexer xs 
lexer ('l':'e':'t':xs) = TokenLet : lexer xs
lexer ('l':'a':'m':'b':'d':'a':xs) = TokenLambda : lexer xs
lexer ('i':'f':xs) = TokenIf : lexer xs
lexer ('c':'o':'n':'d':xs) = TokenCond : lexer xs
lexer ('e':'l':'s':'e':xs) = TokenElse : lexer xs
lexer (x:y:xs) 
  | x == '-' && isDigit y = lexNum2 (x:y:xs)
  | isDigit x = lexNum (x:y:xs)  
lexer ('-':xs) = TokenSub : lexer xs
lexer xs = let (a,b) = varSpan xs in
            (TokenId a) : lexer b 



lexNum :: String -> [Token]
lexNum num = let (a,b) = doubleSpan num
             in  TokenNum (read a): lexer b

lexNum2 :: String -> [Token]
lexNum2 num = let (a,b) = doubleSpan num
           in  TokenNum (read a): lexer b

