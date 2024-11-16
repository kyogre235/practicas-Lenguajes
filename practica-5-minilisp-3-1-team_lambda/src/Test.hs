{-# LANGUAGE TemplateHaskell #-}
module Test (runTests) where

import Lexer
import Parser
import Desugar
import Interp

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Data.AEq
import Data.Char
import Data.Either
import Data.List
import Test.QuickCheck

data Genum = Gand
           | Gor
           | Gnot
           | Glt
           | Ggt
           | Geq
           | Gsum
           | Gsub
           | Gmul
           | Gdiv
           | Gadd1
           | Gsub1
           | Gsqrt
           | Gexpt
           | Glet
           | Glet1
           | Gfun
           | Gif
           | Gcond

fl :: (a -> a -> b -> b) -> (a -> b) -> b -> [a] -> b
fl _ _ b [] = b
fl _ f _ [x] = f x
fl f g b (x:y:xs) = f x y $ fl f g b (y:xs)

nf :: (a -> [a] -> b) -> b -> [a] -> b
nf _ b [] = b
nf f _ (x:xs) = f x xs
                
expr :: Gen (String, Either Bool Double)
expr = do
  n <- chooseInt (0, 5)
  genExpr n

genExpr :: Int -> Gen (String, Either Bool Double)
genExpr n = do
  b <- arbitrary
  if b then do
    (expr, bool) <- genBool n []
    return (expr, Left bool)
    else do
    (expr, num) <- genNum n []
    return (expr, Right num)

genVar :: Gen String
genVar = do
  option <- chooseInt (1,4)
  case option of
    1 -> do
      var <- elements ['a' .. 'z']
      return [var]
    2 -> do
      var <- elements ['a' .. 'z']
      final <- elements "?!"
      return $ var : [final]
    3 -> do
      var1 <- elements ['a' .. 'z']
      var2 <- elements ['a' .. 'z']
      return $ var1 : '-' : [var2]
    4 -> do
      var1 <- elements ['a' .. 'z']
      var2 <- elements ['a' .. 'z']
      final <- elements "?!"
      return $ var1 : '-' : var2 : [final]

type Vars = [(String, Either Bool Double)]

genDefs1Num :: Int -> Int -> Vars -> Gen [(String, String, Double)]
genDefs1Num _ 0 _ = return []
genDefs1Num n k vars = do
  new <- arbitrary
  var <- if new || null vars then
           genVar `suchThat` (not . (`elem` map fst vars))
         else
           fst <$> elements vars
  size <- chooseInt (0, n-1)
  (str, num) <- genNum size vars
  xs <- genDefs1Num n (k-1) $ (var, Right num) : filter ((/= var) . fst) vars
  return $ xs ++ [(var,str,num)]

genFunNum :: Int -> Int -> Vars -> Vars -> Gen [(String, String, Double)]
genFunNum _ 0 _ _ = return []
genFunNum n k vars fvars = do
  new <- arbitrary
  let choosable = filter (not . (`elem` map fst fvars) . fst) vars
  var <- if new || null choosable then
           genVar `suchThat` (not . (`elem` map fst (vars ++ fvars)))
         else
           fst <$> elements choosable
  size <- chooseInt (0, n-1)
  (str, num) <- genNum size vars
  xs <- genFunNum n (k-1) vars $ (var, Right num) : fvars
  return $ (var,str,num) : xs

genNum :: Int -> Vars -> Gen (String, Double)
genNum 0 vars
  | null filteredVars = do
      double <- arbitrary
      return (show double, double)
  | otherwise = do
      new <- arbitrary
      if new then do
        double <- arbitrary
        return (show double, double)
        else do
        (var,value) <- elements filteredVars
        return (var, fromRight 0 value)
  where
    filteredVars = filter (isRight . snd) $ vars
genNum n vars = do
  k <- chooseInt (1,5)
  op <- elements [ Gsum
                 , Gsub
                 , Gmul
                 , Gdiv
                 , Gadd1
                 , Gsub1
                 , Gsqrt
                 , Gexpt
                 , Glet
                 , Glet1
                 , Gfun
                 , Gif
                 , Gcond]
  case op of
    Gif -> do
      (e1,b) <- chooseInt (0,n-1) >>= (`genBool` vars)
      (e2,n2) <- chooseInt (0,n-1) >>= (`genNum` vars)
      (e3,n3) <- chooseInt (0,n-1) >>= (`genNum` vars)
      return ("(if " ++ e1 ++ " " ++ e2 ++ " " ++ e3 ++ ")", if b then n2 else n3)
    Gcond -> do
      l <- vectorOf k $ (\(strb,b) (strn,n) -> ((strb,strn),(b,n))) <$> (chooseInt (0,n-1) >>= (`genBool` vars)) <*> (chooseInt (0,n-1) >>= (`genNum` vars))
      let (conds,values) = unzip l
      (e,n) <- chooseInt (0,n-1) >>= (`genNum` vars)
      let res = foldr (\(b,x) acc -> if b then x else acc) n values
      return ("(cond (" ++ unwords (map (\(x,y) -> "[" ++ x ++ " " ++ y ++ "]") conds) ++ ") (else " ++ e ++ "))", res)
    Gfun -> do
      xs <- genFunNum n k vars []
      size <- chooseInt (0,n-1)
      let newVars = correct $ map (\(a,_,c) -> (a, Right c)) xs
      let oldVars = filter (not . (`elem` map fst newVars) . fst) vars
      (str,num) <- genNum size $ oldVars ++ newVars
      return ( "((lambda (" ++
               intercalate " " (map (\(var,_,_) -> var) xs) ++
               ") " ++ str ++ ") " ++
               intercalate " " (map (\(_,val,_) -> val) xs) ++ ")", num)
    Glet -> do
      xs <- genFunNum n k vars []
      size <- chooseInt (0, n-1)
      let newVars = correct $ map (\(a,_,c) -> (a, Right c)) xs
      let oldVars = filter (not . (`elem` map fst newVars) . fst) vars
      (str,num) <- genNum size $ oldVars ++ newVars
      return ( "(let (" ++
               intercalate " " (map (\(var,val,_) -> "[" ++
                                                     var ++
                                                     " " ++
                                                     val ++
                                                     "]") xs) ++
               ") " ++ str ++ ")", num )
    Glet1 -> do
      xs <- genDefs1Num n k vars
      size <- chooseInt (0, n-1)
      let newVars = correct $ map (\(a,_,c) -> (a, Right c)) xs
      let oldVars = filter (not . (`elem` map fst newVars) . fst) vars
      (str,num) <- genNum size $ oldVars ++ newVars
      return ( "(let* (" ++
               intercalate " " (map (\(var,val,_) -> "[" ++
                                                     var ++
                                                     " " ++
                                                     val ++
                                                     "]") $ reverse xs) ++
               ") " ++ str ++ ")", num )
    Gsum -> do
      xs <- vectorOf k (chooseInt (0, n-1) >>= (`genNum` vars))
      return ("(+ " ++ intercalate " " (map fst xs) ++ ")", foldr (+) 0 $ map snd xs)
    Gsub -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genNum` vars))
      return ("(- " ++ intercalate " " (map fst xs) ++ ")", nf ((. sum) . (-)) 0 $ map snd xs)
    Gmul -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genNum` vars))
      return ("(* " ++ intercalate " " (map fst xs) ++ ")", foldr (*) 1 $ map snd xs)
    Gdiv -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genNum` vars))
      return ("(/ " ++ intercalate " " (map fst xs) ++ ")", foldl1 (/) $ map snd xs)
    Gadd1 -> do
      (e,n) <- chooseInt (0,n-1) >>= (`genNum` vars)
      return ("(add1 " ++ e ++ ")", n + 1)
    Gsub1 -> do
      (e,n) <- chooseInt (0,n-1) >>= (`genNum` vars)
      return ("(sub1 " ++ e ++ ")", n - 1)
    Gsqrt -> do
      (e,n) <- chooseInt (0,n-1) >>= (`genPos` vars)
      return ("(sqrt " ++ e ++ ")", sqrt n)
    Gexpt -> do
      (e1,n1) <- chooseInt (0,n-1) >>= (`genPos` vars)
      (e2,n2) <- chooseInt (0,n-1) >>= (`genNum` vars)
      return ("(expt " ++ e1 ++ " " ++ e2 ++ ")", n1 ** n2)

correct :: [(String, a)] -> [(String, a)]
correct [] = []
correct ((str,a):xs) = (str, a) : (correct $ filter ((/= str) . fst) xs)

genPos :: Int -> Vars -> Gen (String, Double)
genPos n vars = do
  (e,x) <- genNum n vars
  if x < 0 then
    return ("(* -1.0 " ++ e ++")", abs x)
    else
    return (e,x)

genDefs1Bool :: Int -> Int -> Vars -> Gen [(String, String, Bool)]
genDefs1Bool _ 0 _ = return []
genDefs1Bool n k vars = do
  new <- arbitrary
  var <- if new || null vars then
           genVar `suchThat` (not . (`elem` map fst vars))
         else
           fst <$> elements vars
  size <- chooseInt (0, n-1)
  (str, bool) <- genBool size vars
  xs <- genDefs1Bool n (k-1) $ (var, Left bool) : filter ((/= var) . fst) vars
  return $ xs ++ [(var,str,bool)]

genFunBool :: Int -> Int -> Vars -> Vars -> Gen [(String, String, Bool)]
genFunBool _ 0 _ _ = return []
genFunBool n k vars fvars = do
  new <- arbitrary
  let choosable = filter (not . (`elem` map fst fvars) . fst) vars
  var <- if new || null choosable then
           genVar `suchThat` (not . (`elem` map fst (vars ++ fvars)))
         else
           fst <$> elements choosable
  size <- chooseInt (0, n-1)
  (str, bool) <- genBool size vars
  xs <- genFunBool n (k-1) vars $ (var, Left bool) : fvars
  return $ (var,str,bool) : xs

genBool :: Int -> Vars -> Gen (String, Bool)
genBool 0 vars
  | null filteredVars = do
      b <- arbitrary
      return (if b then "#t" else "#f", b)
  | otherwise = do
      new <- arbitrary
      if new then do
        b <- arbitrary
        return (if b then "#t" else "#f", b)
        else do
        (var, value) <- elements filteredVars
        return (var, fromLeft False value)
  where
    filteredVars = filter (isLeft . snd) $ vars
genBool n vars = do
  k <- chooseInt (1,5)
  op <- elements [Gand, Gor, Gnot, Glt, Ggt, Geq, Glet, Glet1, Gfun, Gif, Gcond]
  case op of
    Gif -> do
      (e1,b) <- chooseInt (0,n-1) >>= (`genBool` vars)
      (e2,n2) <- chooseInt (0,n-1) >>= (`genBool` vars)
      (e3,n3) <- chooseInt (0,n-1) >>= (`genBool` vars)
      return ("(if " ++ e1 ++ " " ++ e2 ++ " " ++ e3 ++ ")", if b then n2 else n3)
    Gcond -> do
      l <- vectorOf k $ (\(strb,b) (strn,n) -> ((strb,strn),(b,n))) <$> (chooseInt (0,n-1) >>= (`genBool` vars)) <*> (chooseInt (0,n-1) >>= (`genBool` vars))
      let (conds,values) = unzip l
      (e,n) <- chooseInt (0,n-1) >>= (`genBool` vars)
      let res = foldr (\(b,x) acc -> if b then x else acc) n values
      return ("(cond (" ++ unwords (map (\(x,y) -> "[" ++ x ++ " " ++ y ++ "]") conds) ++ ") (else " ++ e ++ "))", res)
    Gfun -> do
      xs <- genFunBool n k vars []
      size <- chooseInt (0,n-1)
      let newVars = correct $ map (\(a,_,c) -> (a, Left c)) xs
      let oldVars = filter (not . (`elem` map fst newVars) . fst) vars
      (str,bool) <- genBool size $ oldVars ++ newVars
      return ( "((lambda (" ++
               intercalate " " (map (\(var,_,_) -> var) xs) ++
               ") " ++ str ++ ") " ++
               intercalate " " (map (\(_,val,_) -> val) xs) ++ ")", bool)
    Glet -> do
      xs <- genFunBool n k vars []
      size <- chooseInt (0, n-1)
      let newVars = correct $ map (\(a,_,c) -> (a, Left c)) xs
      let oldVars = filter (not . (`elem` map fst newVars) . fst) vars
      (str, bool) <- genBool size $ oldVars ++ newVars
      return ( "(let (" ++
               intercalate " " (map (\(var,val,_) -> "[" ++
                                                     var ++
                                                     " " ++
                                                     val ++
                                                     "]") xs) ++
               ") " ++ str ++ ")", bool )
    Glet1 -> do
      xs <- genDefs1Bool n k vars
      size <- chooseInt (0, n-1)
      let newVars = correct $ map (\(a,_,c) -> (a, Left c)) xs
      let oldVars = filter (not . (`elem` map fst newVars) . fst) vars
      (str, bool) <- genBool size $ oldVars ++ newVars
      return ( "(let* (" ++
               intercalate " " (map (\(var,val,_) -> "[" ++
                                                     var ++
                                                     " " ++
                                                     val ++
                                                     "]") $ reverse xs) ++
               ") " ++ str ++ ")", bool )
    Gand -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genBool` vars))
      return ("(and " ++ intercalate " " (map fst xs) ++ ")", all id $ map snd xs)
    Gor -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genBool` vars))
      return ("(or " ++ intercalate " " (map fst xs) ++ ")", any id $ map snd xs)
    Gnot -> do
      (e,b) <- chooseInt (0,n-1) >>= (`genBool` vars)
      return ("(not " ++ e ++ ")", not b)
    Glt -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genNum` vars))
      b <- arbitrary
      let ys = if b then sortBy (\(_,v1) (_,v2) -> compare v1 v2) xs else xs
      return ( "(< " ++ intercalate " " (map fst ys) ++ ")"
             , fl (((&&) .) . (<)) (const True) True $ map snd ys)
    Ggt -> do
      xs <- vectorOf k (chooseInt (0,n-1) >>= (`genNum` vars))
      b <- arbitrary
      let ys = if b then sortBy (\(_,v1) (_,v2) -> compare v2 v1) xs else xs
      return ( "(> " ++ intercalate " " (map fst ys) ++ ")"
             , fl (((&&) .) . (>)) (const True) True $ map snd ys)
    Geq -> do
      b <- arbitrary
      xs <- if b then
              (take k . repeat) <$> (chooseInt (0,n-1) >>= (`genNum` vars))
            else
              vectorOf k (chooseInt (0,n-1) >>= (`genNum` vars))
      return ("(= " ++ intercalate " " (map fst xs) ++ ")", all (== snd (head xs)) $ map snd (tail xs))

(=~=) :: Double -> Double -> Bool
(=~=) = (~==)

type Lex a = StateT String Maybe a

lexFail :: Lex a
lexFail = StateT $ const Nothing

nxt :: Lex Char
nxt = StateT aux
  where
    aux [] = Nothing
    aux (x:xs) = Just (x,xs)

sat :: (a -> Bool) -> Lex a -> Lex a
sat f p = do
  x <- p
  if f x then return x else lexFail

char :: Char -> Lex Char
char c = sat (== c) nxt

string :: String -> Lex String
string [] = return []
string (x:xs) = do
  c <- char x
  cs <- string xs
  return (c:cs)

token :: Lex a -> Lex a
token p = do
  spaces
  a <- p
  spaces
  return a

tkstr :: String -> Lex String
tkstr = token . string

spaces :: Lex String
spaces = many (sat isSpace nxt)

numD :: Lex String
numD = do
  s <- string "-" <|> return []
  fs <- some $ sat isDigit nxt
  snd <- (do { dot <- string "." ;
               fs <- some $ sat isDigit nxt ;
               snd <- (do { e <- string "e" ;
                            s <- string "-" <|> return [] ;
                            ds <- some $ sat isDigit nxt ;
                            return $ e ++ s ++ ds }) <|> return [] ;
               return $ dot ++ fs ++ snd} ) <|> return []
  return $ show (read $ s ++ fs ++ snd :: Double)

var :: Lex String
var = do
  v <- some $ sat (\x -> isAlpha x || isDigit x || (x == '-')) nxt
  final <- string "?" <|> string "!" <|> return []
  return $ v ++ final

lexT :: Lex [String]
lexT = some $
       tkstr "(" <|>
       tkstr ")" <|>
       tkstr "[" <|>
       tkstr "]" <|>
       tkstr "+" <|>
       token numD <|>
       tkstr "-" <|>
       tkstr "*" <|>
       tkstr "/" <|>
       tkstr "<" <|>
       tkstr ">" <|>
       tkstr "=" <|>
       tkstr "lambda" <|>
       tkstr "if" <|>
       tkstr "cond" <|>
       tkstr "else" <|>
       tkstr "let*" <|>
       tkstr "let" <|>
       tkstr "add1" <|>
       tkstr "sub1" <|>
       tkstr "sqrt" <|>
       tkstr "expt" <|>
       tkstr "not" <|>
       tkstr "and" <|>
       tkstr "or" <|>
       tkstr "#t" <|>
       tkstr "#f" <|>
       token var

genLex :: Gen (String,Maybe [String])
genLex = do
  e <- fst <$> expr
  return (e, fmap fst (runStateT lexT e))

trexpr :: Lex String
trexpr = token numD <|>
         token var <|>
         tkstr "#t" <|>
         tkstr "#f" <|>
         tbin <|>
         tcomp <|>
         tun <|>
         tlet <|>
         tlet1 <|>
         tif <|>
         tcond <|>
         tfun <|>
         tap
  where
    tif = do
      tkstr "("
      tkstr "if"
      b <- trexpr
      t <- trexpr
      e <- trexpr
      tkstr ")"
      return $ "(if " ++ b ++ " " ++ t ++ " " ++ e ++ ")"
    tcond = do
      tkstr "("
      tkstr "cond"
      tkstr "("
      conds <- many $ condition
      tkstr ")"
      tkstr "("
      tkstr "else"
      e <- trexpr
      tkstr ")"
      tkstr ")"
      return $ foldr (\(a,b) acc -> "(if "++ a ++ " " ++ b ++ " " ++ acc ++ ")") e conds
    condition = do
      tkstr "["
      a <- trexpr
      b <- trexpr
      tkstr "]"
      return (a,b)
    tbin = do
      tkstr "("
      op <- binops
      args <- many trexpr
      tkstr ")"
      case args of
        [] -> lexFail
        [x] -> return x
        (x:y:xs) -> return $
                    foldl (\acc x -> "(" ++
                                     intercalate " " [op,acc,x] ++
                                     ")")
                    ("(" ++ intercalate " " [op,x,y] ++")") xs
    tcomp = do
      tkstr "("
      op <- comp
      args <- many trexpr
      tkstr ")"
      case args of
        [] -> lexFail
        xs -> return $ acomp op xs
    acomp _ [_] = "#t"
    acomp op [x,y] = "(" ++ intercalate " " [op,x,y] ++ ")"
    acomp op (x:y:xs) = "(and (" ++ intercalate " " [op,x,y] ++ ") " ++ acomp op (y:xs) ++ ")"
    tun = do
      tkstr "("
      op <- unops
      args <- many trexpr
      tkstr ")"
      case args of
        [x] -> return $ "(" ++ op ++ " " ++ x ++ ")"
        _ -> lexFail
    tlet = do
      tkstr "("
      tkstr "let"
      tkstr "("
      decls <- some $ tkstr "[" >> token var >>= (trexpr >>=) . ((tkstr "]" >>) .) . (return .) . (,)
      tkstr ")"
      expr <- trexpr
      tkstr ")"
      return $
        foldl (\acc x ->
                  "(" ++ acc ++ " " ++ x ++ ")")
        (foldr (\x acc ->
                   "(lambda (" ++ x ++ ") " ++ acc ++ ")")
         expr (map fst decls)) (map snd decls)
    tlet1 = do
      tkstr "("
      tkstr "let*"
      tkstr "("
      decls <- some $ tkstr "[" >> token var >>= (trexpr >>=) . ((tkstr "]" >>) .) . (return .) . (,)
      tkstr ")"
      expr <- trexpr
      tkstr ")"
      return $ foldr (\(x,v) acc -> "((lambda (" ++ x ++ ") " ++ acc ++ ") " ++ v ++ ")") expr decls
    tfun = do
      tkstr "("
      tkstr "lambda"
      tkstr "("
      vars <- many $ token var
      tkstr ")"
      expr <- trexpr
      tkstr ")"
      return $ foldr (\x acc -> "(lambda (" ++ x ++ ") " ++ acc ++ ")") expr vars
    tap = do
      tkstr "("
      fun <- trexpr
      args <- many trexpr
      tkstr ")"
      case args of
        [] -> lexFail
        (x:xs) -> return $ foldl (\acc x -> "(" ++ acc ++ " " ++ x ++ ")") ("(" ++ fun ++ " " ++ x ++ ")") xs
    binops = tkstr "+" <|>
             tkstr "-" <|>
             tkstr "*" <|>
             tkstr "/" <|>
             tkstr "expt" <|>
             tkstr "and" <|>
             tkstr "or"
    comp = tkstr "<" <|>
           tkstr ">" <|>
           tkstr "="
    unops = tkstr "add1" <|>
            tkstr "sub1" <|>
            tkstr "sqrt" <|>
            tkstr "not"

genTranslate :: Gen (String, Maybe String)
genTranslate = do
  e <- fst <$> expr
  return (e, fmap fst (runStateT trexpr e))

prop_lexer_ok :: Property
prop_lexer_ok = forAll genLex
                (\(e,mxs) -> counterexample
                  ("El resultado de:\nJust $ map show $ lexer " ++ show e ++
                   "\nDebería ser:\n" ++ show mxs ++
                   "\nY regresó:\n" ++ show (Just $ map show $ lexer e)) $
                  Just (map show (lexer e)) == mxs)

prop_parser_ok :: Property
prop_parser_ok = forAll (fst <$> expr)
                 (\e -> counterexample
                   ("El resultado de:\nshow $ parser $ lexer " ++ show e ++
                    "\nDebería ser:\n" ++ show e ++
                    "\nY regresó:\n" ++ show (show $ parser $ lexer e)) $ show (parser (lexer e)) == e)

prop_desugar_ok :: Property
prop_desugar_ok = forAll genTranslate
                  (\(e,tre) -> counterexample
                    ("El resultado de:\nJust $ show $ desugar $ parser $ lexer " ++
                     show e ++
                     "\nDebería ser:\n" ++ show tre ++
                     "\nY regresó:\n" ++
                     show (Just $ show $ desugar $ parser $ lexer e)) $
                    (Just $ show $ desugar $ parser $ lexer e) == tre)
  
prop_interp_ok :: Property
prop_interp_ok = forAll expr check
  where
    check (str, Left b) =
      let
        eval = bool $ strict $ interp (desugar $ parser $ lexer str) []
      in
        counterexample ("El resultado de:\nbool $ strict $ interp (desugar $ parser $ lexer " ++ show str ++ ") []" ++
                        "\nDebería ser:\n" ++ show b ++
                        "\nY regresó:\n" ++ show eval) $ eval == b
    check (str, Right n) =
      let
        eval = num $ strict $ interp (desugar $ parser $ lexer str) []
      in
        counterexample ("El resultado de:\n num $ strict $ interp (desugar $ parser $ lexer " ++ show str ++ ") []" ++
                        "\nDebería ser aproximadamente:\n" ++ show n ++
                        "\nY regresó:\n" ++ show eval) $
        isInfinite eval && isInfinite n ||
        isNaN eval && isNaN n ||
        (eval =~= n)

return []
runTests n = $forAllProperties (quickCheckResult . withMaxSuccess n)
