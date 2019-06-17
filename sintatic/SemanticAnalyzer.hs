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
        IntLit _ v -> (memory, (IntType, Int v))
        

assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId (Memory table io) (AtomicToken (Id _ var)) expr = -- mem1 
    Memory table2 ((print (var)) >> io2)
        where
            ((Memory table io), (type1, value1)) = evaluateExpr (Memory table io) expr
            (Memory table2 io2) = memory_assign (Variable var type1 value1) (Memory table io)
        
emptyMemory :: Memory
emptyMemory = (Memory [] (return()))

initialize :: ExprTree -> IO() 
initialize tree = getFinalMemoryIO (semanticAnalyzer tree emptyMemory)

getFinalMemoryIO :: Memory -> IO()
getFinalMemoryIO (Memory table io) = io

semanticAnalyzer :: ExprTree -> Memory -> Memory 
--semanticAnalyzer (SingleNode NonTStatement a) (Memory table io) = 
--    Memory table2 ((print ("SingleNode NonTStatement", a)) >> io2)
--        where
--            (Memory table2 io2) = semanticAnalyzer a (Memory table io)

-- assign
semanticAnalyzer (DoubleNode NonTAssign a b) memory =
     assignToId memory a b
    --assignToId memory a b  

-- statements 
semanticAnalyzer (DoubleNode _ a b) memory =
    semanticAnalyzer b memory1
    where
       memory1 = semanticAnalyzer a memory
       
semanticAnalyzer (SingleNode _ a) memory = 
    semanticAnalyzer a memory

semanticAnalyzer (AtomicToken atomic) (Memory table io) = 
    Memory table ((print atomic) >> io)


main :: IO ()
main = do
    case unsafePerformIO (parser (getTokens "problem1.ml")) of
    {
        Left err -> print err;
        Right ans -> initialize ans
    }