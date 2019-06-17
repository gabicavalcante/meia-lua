import Tokens
import Expression
import Memory
import ParserTokensNTerminal

import System.IO.Unsafe
import System.Environment



memory_assign :: Variable -> Memory -> Memory
memory_assign variable (Memory [] io) = Memory [variable] io
memory_assign (Variable id1 type1 value1) (Memory((Variable id2 type2 value2) : t) io) =
                                if id1 == id2 then append_memory (Variable id2 type2 value1) (Memory t io)
                                else append_memory (Variable id2 type2 value2) (memory_assign (Variable id1 type1 value1) (Memory t io))

append_memory :: Variable -> Memory -> Memory
append_memory variable (Memory [] io) = Memory [variable] io
append_memory variable (Memory variables io) = Memory (variable : variables) io

lookUpVariable :: String -> Memory -> Variable
lookUpVariable id1 (Memory((Variable id2 type2 value2) : t) io) =
                                if id1 == id2 then (Variable id1 type2 value2)
                                else lookUpVariable id1 (Memory t io)
lookUpVariable id1 (Memory [] io) =  error "Variavel não encontrada"

evaluateExpr :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateExpr memory exprTree = case exprTree of
    -- atomics
    AtomicToken a -> case a of
        IntLit _ v      -> (memory, (IntType, Int v))
        FloatLit _ v    -> (memory, (FloatType, Float v))
        StrLit _ v      -> (memory, (StringType, String v)) 
    -- print
    SingleNode nonT a -> case nonT of
        NonTId -> evaluateSingleNode memory a
    -- expression
    TripleNode nonT a b c -> case nonT of
        NonTExpr -> evaluateOp memory a b c

evaluateSingleNode :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateSingleNode memory (AtomicToken (Id _ id)) = res
    where
        (Variable id1 typ val) = lookUpVariable id memory
        res = (memory, (typ, val))
--evaluateSingleNode memory (SingleNode a) = (evaluateSingleNode memory a)

evaluateOp :: Memory -> ExprTree -> ExprTree -> ExprTree -> (Memory, (Type, Value))
-- Adicao :   a + b
evaluateOp mem a (AtomicToken (SymOpPlus _)) b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, opSum (type1, val1) (type2, val2))
-- Subtracao :  a - b
evaluateOp mem a (AtomicToken (SymOpMinus _)) b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, opMinus (type1, val1) (type2, val2))
-- Multiplicacao : a * b
evaluateOp mem a (AtomicToken (SymOpMult _)) b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, opMult (type1, val1) (type2, val2))

toFloat :: Int -> Float
toFloat a = fromInteger (toInteger a)

opSum :: (Type, Value) -> (Type, Value) -> (Type, Value)
opSum (IntType, Int a) (IntType, Int b) = (IntType, Int (a + b))
opSum (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a + b))
opSum (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( toFloat a + b ))
opSum (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a + toFloat b ))

opMinus :: (Type, Value) -> (Type, Value) -> (Type, Value)
opMinus (IntType, Int a) (IntType, Int b) = (IntType, Int (a - b))
opMinus (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a - b))
opMinus (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( toFloat a - b ))
opMinus (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a - toFloat b ))

opMult :: (Type, Value) -> (Type, Value) -> (Type, Value)
opMult (IntType, Int a) (IntType, Int b) = (IntType, Int (a * b))
opMult (FloatType, Float a) (FloatType, Float b) = (FloatType, Float (a * b))
opMult (IntType, Int a) (FloatType, Float b) = (FloatType, Float ( toFloat a * b ))
opMult (FloatType, Float a) (IntType, Int b) = (FloatType, Float ( a * toFloat b ))

assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId (Memory table io) (AtomicToken (Id _ var)) expr = -- mem1 
    Memory table2 ((print (table)) >> io2)
        where
            (men, (type1, value1)) = evaluateExpr (Memory table io) expr
            (Memory table2 io2) = memory_assign (Variable var type1 value1) men

printAll :: Memory -> ExprTree -> Memory 
printAll memory (SingleNode NonTParams expr) = 
    (Memory table io) --((print (var_type, val)) >> io))
        where
            ((Memory table io2), (var_type, val)) = evaluateExpr memory expr
            io = io2 >> (printValue val) >> (putStrLn "")

printValue :: Value -> IO()
printValue val = case val of
    (Int a) -> putStr ((show a)++" ") 
    (Float a) -> putStr ((show a)++" ") 
    (String a) -> putStr a
    (Bool a) -> putStr ((show a)++" ")   
    
initialize :: ExprTree -> IO() 
initialize tree = getFinalMemoryIO (semanticAnalyzer tree (Memory [] (return())))

getFinalMemoryIO :: Memory -> IO()
getFinalMemoryIO (Memory table io) = ((print (table)) >> io)

semanticAnalyzer :: ExprTree -> Memory -> Memory   
--semanticAnalyzer (SingleNode NonTStatement a) (Memory table io) = 
--    Memory table2 ((print ("SingleNode NonTStatement", a)) >> io2)
--        where
--            (Memory table2 io2) = semanticAnalyzer a (Memory table io)

-- program
semanticAnalyzer (SingleNode NonTProgram a) (Memory table io) = 
    Memory table2 io2 --((print (a)) >> io2)
        where
            (Memory table2 io2) = semanticAnalyzer a (Memory table io)
        

-- statements  
semanticAnalyzer (DoubleNode NonTStatements a b) memory =  
    semanticAnalyzer b memory1
        where 
            memory1 = semanticAnalyzer a memory
            
semanticAnalyzer (SingleNode NonTStatement a) memory = 
    semanticAnalyzer a memory

-- assign
semanticAnalyzer (DoubleNode NonTAssign a b) memory =
    Memory table2 io2
        where 
            (Memory table2 io2) = assignToId memory a b 

semanticAnalyzer (SingleNode NonTPrint params) memory = 
    printAll memory params


main :: IO ()
main = do
    case unsafePerformIO (parser (getTokens "problem1.ml")) of
    {
        Left err -> print err;
        Right ans -> initialize ans
    }