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
             -- (memory_assign (Variable(IntType v)), (IntType, Int v)) 

assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId (Memory table io) id expr = --memory1
    Memory table2 ((print ("assignToId", table)) >> io2)
        where
            ((Memory table2 io2), exprRes) = evaluateExpr (Memory table io) expr

emptyMemory :: Memory
emptyMemory = (Memory [] (return()))

initialize :: ExprTree -> IO() 
initialize tree = getFinalMemoryIO (semanticAnalyzer tree emptyMemory)

getFinalMemoryIO :: Memory -> IO()
getFinalMemoryIO (Memory _ io) = io

semanticAnalyzer :: ExprTree -> Memory -> Memory 
semanticAnalyzer (SingleNode NonTStatement a) memory = semanticAnalyzer a memory
--semanticAnalyzer (SingleNode NonTStatement a) (Memory table io) = 
--    Memory table2 ((print ("SingleNode NonTStatement", a)) >> io2)
--        where
--            (Memory table2 io2) = semanticAnalyzer a (Memory table io)

semanticAnalyzer (DoubleNode NonTAssign a c) memory = assignToId memory a c
--semanticAnalyzer (DoubleNode NonTAssign a b) memory =
--    Memory table ((print (a, b)) >> io)
--            where
--                (Memory table io) = semanticAnalyzer b memory
--                memory1 = semanticAnalyzer a memory

semanticAnalyzer (DoubleNode NonTStatements a b) memory =  
    Memory table ((print ("DoubleNode NonTStatements", a, b)) >> io)
            where
                (Memory table io) = semanticAnalyzer b memory
                memory1 = semanticAnalyzer a memory
            
semanticAnalyzer (SingleNode NonTProgram a) (Memory table io) = 
   Memory table2 ((print ("SingleNode NonTProgram", a)) >> io2)
      where
           (Memory table2 io2) = semanticAnalyzer a (Memory table io)
                
semanticAnalyzer (AtomicToken atomic) (Memory table io) = 
    Memory table ((print atomic) >> io)


main :: IO ()
main = do
    case unsafePerformIO (parser (getTokens "problem1.ml")) of
    {
        Left err -> print err;
        Right ans -> initialize ans
    }