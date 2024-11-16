module Interp where

import Desugar
import Parser

type Env = [(String,Value)]

data Value = NumV Double
            | BooV Bool 
            | CloV String ASA Env
            | StrV ASA Env

num :: Value -> Double
num (NumV n) = n
num _ = error "no es un numero"

bool :: Value -> Bool
bool (BooV b) = b
bool _ = error "no es un booleano"

instance Show Value where
  show (NumV n) = show n 
  show (BooV b) = if b then "#t" else "#f"
  show (CloV s v e) = "< " ++ s ++ ", " ++ show v ++ ", " ++ "e" ++ " >" 
  show (StrV v e) = "#<procedure>"
 

interp :: ASA -> Env -> Value
interp (DNum n) _ = NumV n
interp (DBoo b) _ = BooV b
interp (DStr s) env = buscarValor s env 
interp (DAdd x y) e = NumV $ (num $ interp x e) + (num $ interp y e)
interp (DSub x y) e = NumV $ (num $ interp x e) - (num $ interp y e)
interp (DMul x y) e = NumV $ (num $ interp x e) * (num $ interp y e)
interp (DDiv x y) e = NumV $ (num $ interp x e) / (num $ interp y e)
interp (DAd1 x) e = NumV $ (num $ interp x e) + 1
interp (DSu1 x) e = NumV $ (num $ interp x e) - 1
interp (DSqt x) e = NumV $ sqrt (num $ interp x e)
interp (DExp x y) e = NumV $ (num $ interp x e) ** (num $ interp y e)
interp (DMaq x y) e = BooV $ (num $ interp x e) > (num $ interp y e)
interp (DMeq x y) e = BooV $ (num $ interp x e) < (num $ interp y e)
interp (DEqu x y) e = BooV $ (num $ interp x e) == (num $ interp y e)
interp (DNot x) e = BooV $ not (bool $ interp x e) 
interp (DAnd x y) e = BooV $ (bool $ interp x e) && (bool $ interp y e)
interp (DOrb x y) e = BooV $ (bool $ interp x e) || (bool $ interp y e)
interp (DIft c t e) env = if c1 then interp t env else interp e env
                          where
                            c1 = bool $ strict $ interp c env
interp (DLam (DStr p) c) e = CloV p c e
interp (DApp f a) env = interp c e' 
                  where
                    e' = [(p,(StrV a env))] ++ e
                    CloV p c e = strict f' 
                    f' =  interp f env 


buscarValor :: String -> Env -> Value 
buscarValor clave lista = case lookup clave lista of
    Just valor -> strict valor
    Nothing    -> error $ "Clave " ++ show clave ++ " no encontrada."


strict :: Value -> Value
strict (NumV n) = (NumV n)
strict (BooV b) = (BooV b)
strict (CloV s a e) = CloV s a e
strict (StrV a e) = interp a e
