module Repl where

import Parser
import Lexer
import Desugar
import Interp
import Test

import Control.Exception
import Control.Monad.Trans.Class
import Data.Char
import System.Console.Haskeline
import Text.Read (readMaybe)

-- Función encargada de llevar la ejecución del programa mediante los siguientes pasos:
-- 1. Impresión del propt.
-- 2. Lectura de una cadena.
-- 3. Si la cadena es igual a ":q", se cierra el intérprete.
-- 4. En caso contrario, realiza la generación de código ejecutable aplicando los análisis en
--    orden siguiente: léxico, sintáctico, semántico.
-- 5. Vuelve a ejecutar el ciclo.
repl :: InputT IO ()
repl = do
  minput <- getInputLine "> "
  case (words <$> minput, minput) of
    (Nothing, _) -> return ()
    (Just [], _) -> repl
    (Just [":q"], _) -> return ()
    (Just ["test",num], _) ->
      case (readMaybe num :: Maybe Int) of
        Nothing -> outputStrLn "El parámetro debe ser un número" >> repl
        Just n -> lift (runTests n >> return ()) >> repl
    (Just ("test":_), _) -> outputStrLn "El modo de uso es: test <numero_de_pruebas>" >> repl
    (_, Just xs) -> lift (catch (putStrLn $ show $ strict $ interp (desugar $ parser $ lexer xs) []) (\(e :: ErrorCall) -> putStrLn $ displayException e)) >> repl
    

-- Función principal. Da la bienvenida al usuario y ejecuta el REPL.
main = do
  putStrLn "Mini-Lisp v3.0. Bienvenido." 
  runInputT defaultSettings repl
