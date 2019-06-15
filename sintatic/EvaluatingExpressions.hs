import Tokens
import Expression
import ParserTokens
import Memory

import System.IO.Unsafe
import System.Environment 

assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId st id (UniTree (AtomicToken typ)) = finalState
    where
        valor = getInputOfType typ
        (st1, exprRes) = (st, valor) 
assignToId st id expr = finalState
    where
        (st1, exprRes) = evaluateExpr st expr 


inicAnalisadorSemantico :: ExprTree -> IO()
inicAnalisadorSemantico tree = getAnalisadorIO (analisadorSemantico tree emptyState)

getAnalisadorIO :: Memory -> IO()
getAnalisadorIO (Memory _ io _) = io

analisadorSemantico :: ExprTree -> Memory -> Memory
-- assign
analisadorSemantico (DualTree a c) st = assignToId st a c 

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [file] -> do 
            -- fn <- readFile file
            case unsafePerformIO (parser (getTokens file)) of
                { Left err -> print err; 
                  Right ans -> inicAnalisadorSemantico ans
                }
        _ -> do
            case unsafePerformIO (parser (getTokens "problem1.ml")) of
                { Left err -> print err; 
                  Right ans -> inicAnalisadorSemantico ans
                }
