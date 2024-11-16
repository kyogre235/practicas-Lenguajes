module Desugar where

import Parser

data ASA = DNum Double
          | DStr String
          | DBoo Bool
          | DAdd ASA ASA
          | DSub ASA ASA
          | DMul ASA ASA
          | DDiv ASA ASA
          | DAd1 ASA
          | DSu1 ASA
          | DSqt ASA
          | DExp ASA ASA
          | DMaq ASA ASA
          | DMeq ASA ASA
          | DEqu ASA ASA
          | DNot ASA
          | DAnd ASA ASA
          | DOrb ASA ASA
          | DLam ASA ASA
          | DApp ASA ASA
          | DIft ASA ASA ASA 

instance Show ASA where
  show (DStr s) = s
  show (DNum d) = show d
  show (DBoo b) = if b then "#t" else "#f"
  show (DAdd x y) = "(" ++ "+ " ++ show x ++ " " ++ show y ++ ")"
  show (DSub x y) = "(" ++ "- " ++ show x ++ " " ++ show y ++ ")"
  show (DMul x y) = "(" ++ "* " ++ show x ++ " " ++ show y ++ ")"
  show (DDiv x y) = "(" ++ "/ " ++ show x ++ " " ++ show y ++ ")"
  show (DAd1 x) = "(" ++ "add1 " ++ show x ++ ")"
  show (DSu1 x) = "(" ++ "sub1 " ++ show x ++ ")"
  show (DSqt x) = "(" ++ "sqrt " ++ show x ++ ")"
  show (DExp x y) = "(" ++ "expt " ++ show x ++ " " ++ show y ++ ")"
  show (DMaq x y) = "(" ++ "> " ++ show x ++ " " ++ show y ++ ")"
  show (DMeq x y) = "(" ++ "< " ++ show x ++ " " ++ show y ++ ")"
  show (DEqu x y) = "(" ++ "= " ++ show x ++ " " ++ show y ++ ")"
  show (DNot x) = "(" ++ "not " ++ show x ++ ")"
  show (DAnd x y) = "(" ++ "and " ++ show x ++ " " ++ show y ++ ")"
  show (DOrb x y) = "(" ++ "or " ++ show x ++ " " ++ show y ++ ")"
  show (DLam x y) = "(" ++ "lambda " ++ "(" ++ show x ++ ") " ++ show y ++ ")"
  show (DApp x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (DIft x y z) = "(" ++ "if " ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"

desugar :: SASA -> ASA
desugar (Str s) = DStr s
desugar (Num n) = DNum n
desugar (Boo b) = DBoo b

desugar (Add [x,y]) = DAdd (desugar x) (desugar y)
desugar (Add [x]) = desugar x
desugar (Add (x:y:xs)) = desugar $ Add ([Add [x,y]] ++ xs)

desugar (Sub [x,y]) = DSub (desugar x) (desugar y)
desugar (Sub [x]) = desugar x
desugar (Sub (x:y:xs)) = desugar $ Sub ([Sub [x,y]] ++ xs)

desugar (Mul [x,y]) = DMul (desugar x) (desugar y)
desugar (Mul [x]) = desugar x
desugar (Mul (x:y:xs)) = desugar $ Mul ([Mul [x,y]] ++ xs)

desugar (Div [x,y]) = DDiv (desugar x) (desugar y)
desugar (Div [x]) = desugar x
desugar (Div (x:y:xs)) = desugar $ Div ([Div [x,y]] ++ xs)

desugar (Ad1 [x]) = DAd1 (desugar x)
desugar (Su1 [x]) = DSu1 (desugar x)
desugar (Sqt [x]) = DSqt (desugar x)
desugar (Exp [x,y]) = DExp (desugar x) (desugar y)

desugar (Maq [x,y]) = DMaq (desugar x) (desugar y)
desugar (Maq [x]) = DBoo True
desugar (Maq (x:y:xs)) = DAnd (desugar (Maq [x, y])) (desugar $ Maq (y:xs))

desugar (Meq [x,y]) = DMeq (desugar x) (desugar y)
desugar (Meq [x]) = DBoo True
desugar (Meq (x:y:xs)) = DAnd (desugar (Meq [x, y])) (desugar $ Meq (y:xs))

desugar (Equ [x,y]) = DEqu (desugar x) (desugar y)
desugar (Equ [x]) = DBoo True
desugar (Equ (x:y:xs)) = DAnd (desugar (Equ [x, y])) (desugar $ Equ (y:xs))

desugar (Not [x]) = DNot (desugar x)

desugar (And [x,y]) = DAnd (desugar x) (desugar y)
desugar (And [x]) = desugar x
desugar (And (x:y:xs)) = desugar $ And ([And [x,y]] ++ xs)   

desugar (Orb [x,y]) = DOrb (desugar x) (desugar y)
desugar (Orb [x]) =  desugar x
desugar (Orb (x:y:xs)) = desugar $ Orb ([Orb [x,y]] ++ xs)

desugar (Ift x y z) = DIft (desugar x) (desugar y) (desugar z)
desugar (Con [(x,y)] z) = desugar $ Ift x y z
desugar (Con (x:xs) z) = DIft (desugar $ fst x) (desugar $ snd x) (desugar $ Con xs z)

desugar (Lam [] x) = desugar x
desugar (Lam (x:xs) y) = DLam (desugar x) (desugar $ Lam xs y)
desugar (App f xs) = aux (desugar f) xs
                    where
                      aux :: ASA -> [SASA] -> ASA
                      aux a [] = a
                      aux a (x:xs) = aux (DApp a (desugar x)) xs
                      
desugar (Let [] a) = desugar a
desugar (Let  y a) = desugar $ App (Lam (desugarVars y) a) (desugarVars2 y) 
desugar (Le1 [] a) = desugar a
desugar (Le1 ((Str s,asa):xs) y) = DApp (DLam (DStr s) (desugar $ Le1 xs y)) (desugar asa)

desugarVars :: [(SASA, SASA)] -> [SASA]
desugarVars = map fst

desugarVars2 :: [(SASA, SASA)] -> [SASA]
desugarVars2 = map snd
