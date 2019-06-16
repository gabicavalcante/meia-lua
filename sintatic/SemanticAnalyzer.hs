import Tokens
import Expression
import Memory
import ParserTokensNTerminal

import System.IO.Unsafe
import System.Environment

evaluateExpr :: Memory -> ExprTree -> Memory
evaluateExpr memory exprTree = case exprTree of
    -- atomics
    AtomicToken a -> case a of
        IntLit _ v -> emptyMemory
        --IntLit _ v -> updateState(memory_assign (Variable(IntType v)))
        --IntLit _ v -> (memory, (IntType, Int v))
        --FloatLit _ v -> (memory, (FloatType, Float v))
        --StrLit _ v -> (memory, (StringType, String v))
        --SymTrue _ -> (memory, (BoolType, Bool True))
        --SymFalse _ -> (memory, (BoolType, Bool False))

assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId st id expr = st1
    where
        st1 = evaluateExpr st expr

emptyMemory :: Memory
emptyMemory = (Memory [] (return()))

initialize :: ExprTree -> IO()
--initialize tree = semanticAnalyzer tree emptyMemory
initialize tree = getFinalMemoryIO (semanticAnalyzer tree emptyMemory)

getFinalMemoryIO :: Memory -> IO()
getFinalMemoryIO (Memory _ io) = io  

semanticAnalyzer :: ExprTree -> Memory -> Memory 
semanticAnalyzer (DoubleNode NonTAssign a b) memory =
    Memory table ((print (a, b)) >> io)
            where
                (Memory table io) = semanticAnalyzer b memory
                memory1 = semanticAnalyzer a memory

semanticAnalyzer (DoubleNode NonTStatements a b) memory =  
    Memory table ((print ("DoubleNode NonTStatements", a, b)) >> io)
            where
                (Memory table io) = semanticAnalyzer b memory
                memory1 = semanticAnalyzer a memory

semanticAnalyzer (SingleNode NonTStatement a) (Memory table io) = 
    Memory table2 ((print ("SingleNode NonTStatement", a)) >> io2)
        where
            (Memory table2 io2) = semanticAnalyzer a (Memory table io)
            
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