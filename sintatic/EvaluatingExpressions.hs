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
        --IntLit _ v -> emptyMemory
        --IntLit _ v -> updateState(memory_assign (Variable(IntType v)))
        IntLit _ v -> (memory, (IntType, Int v))
        FloatLit _ v -> (memory, (FloatType, Float v))
        StrLit _ v -> (memory, (StringType, String v))
        SymTrue _ -> (memory, (BoolType, Bool True))
        SymFalse _ -> (memory, (BoolType, Bool False))

    SingleNode a -> evaluateSingleNode memory a

    --DoubleNode (AtomicToken (SymBoolNot _)) b -> res
    --    where
    --        (mem1, (type1, val1)) = evaluateExpr memory b
    --        res = (st1, exprBoolNot (type1, val1))

    TripleNode a b c -> evaluateTriTree memory a b c

evaluateSingleNode :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateSingleNode memory (AtomicToken (Id _ id)) = res
    where
        (id1,typ,val) = lookUpVariable id memory
        res = (memory, (typ, val))
evaluateSingleNode memory (SingleNode a) = (evaluateSingleNode memory a)

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


intToFloat :: Int -> Float
intToFloat a = fromInteger (toInteger a)

exprSum :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprSum (IntType, Int a) (IntType, Int b) = (IntType, Int (a + b))
exprSum (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a + b))
exprSum (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a + b ))
exprSum (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a + intToFloat b ))
exprSum (StringType, String a) (StringType, String b) = (StringType, String (a ++ b))
exprSum a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprMinus :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprMinus (IntType, Int a) (IntType, Int b) = (IntType, Int (a - b))
exprMinus (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a - b))
exprMinus (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a - b ))
exprMinus (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a - intToFloat b ))
exprMinus a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprMult :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprMult (IntType, Int a) (IntType, Int b) = (IntType, Int (a * b))
exprMult (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a * b))
exprMult (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a * b ))
exprMult (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a * intToFloat b ))
exprMult a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprDiv :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprDiv (IntType, Int a) (IntType, Int b) = (IntType, Int (a `div` b))
exprDiv (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a / b))
exprDiv (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a / b ))
exprDiv (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a / intToFloat b ))
exprDiv a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprMod :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprMod (IntType, Int a) (IntType, Int b) = (IntType, Int (a `rem` b))
exprMod a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

exprExp :: (Type, Value) -> (Type, Value) -> (Type, Value)
exprExp (IntType, Int a) (IntType, Int b) = (IntType, Int (a ^ b))
exprExp (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a ** b))
exprExp (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( intToFloat a ** b ))
exprExp (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a ** intToFloat b ))
exprExp a b = error ("Operação entre os tipos " ++ (show a) ++ " e " ++ (show b) ++ " não é permitida")

assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId mem id expr = mem1
    where
        (mem, (type1, value1)) = evaluateExpr mem expr
        mem1 = memory_assign Variable(id, type1, value1) mem

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