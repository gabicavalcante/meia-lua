import Tokens
import Expression
import Memory
import ParserTokensNTerminal

import System.IO.Unsafe
import System.Environment

evaluateExpr :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateExpr memory exprTree = case exprTree of
    -- atomics
    AtomicToken a -> case a of
        IntLit _ v -> emptyMemory
        IntLit _ v -> updateState(memory_assign (Variable(IntType v)))
        IntLit _ v -> (memory, (IntType, Int v))
        FloatLit _ v -> (memory, (FloatType, Float v))
        StrLit _ v -> (memory, (StringType, String v))
        SymTrue _ -> (memory, (BoolType, Bool True))
        SymFalse _ -> (memory, (BoolType, Bool False))

    SingleNode a -> evaluateSingleNode st a

    --DoubleNode (AtomicToken (SymBoolNot _)) b -> res
    --    where
    --        (mem1, (type1, val1)) = evaluateExpr memory b
    --        res = (st1, exprBoolNot (type1, val1))

    TripleNode a b c -> evaluateTriTree memory a b c

evaluateSingleNode :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateSingleNode memory (AtomicToken (Id _ id)) = res
    where
        (Variable (typ val )) = lookUpVariable memory id
        res = (memory, (typ, val))
evaluateSingleNode memory (SingleNode a) = (evaluateSingleNode st a)

evaluateTriTree :: Memory -> ExprTree -> ExprTree -> ExprTree -> (Memory, (Type, Value))
-- Adicao :   a + b
evaluateTriTree mem (AtomicToken (SymOpPlus _)) a b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, exprSum (type1, val1) (type2, val2))

-- Subtracao :  a - b
evaluateTriTree mem (AtomicToken (SymOpMinus _)) a b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, exprMinus (type1, val1) (type2, val2))

-- Multiplicacao : a * b
evaluateTriTree mem (AtomicToken (SymOpMult _)) a b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, exprMult (type1, val1) (type2, val2))


assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId st id expr = st1
    where
        st1 = evaluateExpr st expr

emptyMemory :: Memory
emptyMemory = (Memory [] (return()))

inicAnalisadorSemantico :: ExprTree -> IO()
inicAnalisadorSemantico tree = getFinalMemoryIO (analisadorSemantico tree emptyMemory)

getFinalMemoryIO :: Memory -> IO()
getFinalMemoryIO (Memory _ io) = io
--(return ())

analisadorSemantico :: ExprTree -> Memory -> Memory
-- assign
analisadorSemantico (DoubleNode a c) st = assignToId st a c

getInput :: String
getInput = unsafePerformIO (getLine)

getInputOfType :: Token -> (Type, Value)
getInputOfType (TypeInt _) = (IntType, Int value)
    where
        value = read getInput :: Int
getInputOfType (TypeFloat _) = (FloatType, Float value)
    where
        value = read getInput :: Float
getInputOfType (TypeString _) = (StringType, String value)
    where
        value = getInput :: String
getInputOfType _ = error "Operação de scan não permitida para esse tipo"


main :: IO ()
main = do
    case unsafePerformIO (parser (getTokens "problem1.ml")) of
    {
        Left err -> print err;
        Right ans -> inicAnalisadorSemantico ans
    }