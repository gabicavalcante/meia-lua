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
        SymTrue _ -> (memory, (BoolType, Bool True))
        SymFalse _ -> (memory, (BoolType, Bool False))
 
    SingleNode nonT a -> case nonT of
        NonTId -> evaluateSingleNode memory a

evaluateSingleNode :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateSingleNode memory (AtomicToken (Id _ id)) = res
    where
        (Variable id1 typ val) = lookUpVariable id memory
        res = (memory, (typ, val))
--evaluateSingleNode memory (SingleNode a) = (evaluateSingleNode memory a)


assignToId :: Memory -> ExprTree -> ExprTree -> Memory
assignToId (Memory table io) (AtomicToken (Id _ var)) expr = -- mem1 
    Memory table2 ((print (table)) >> io2)
        where
            (men, (type1, value1)) = evaluateExpr (Memory table io) expr
            (Memory table2 io2) = memory_assign (Variable var type1 value1) men

printAll :: Memory -> ExprTree -> Memory 
printAll memory (SingleNode NonTParams expr) = 
    (Memory midTable ((print (var_type, val)) >> finalIO))
        where
            ((Memory midTable midIO), (var_type, val)) = evaluateExpr memory expr
            finalIO = midIO >> (printOne val) >> (putStrLn "")

printOne :: Value -> IO()
printOne val = case val of
    (Int a) -> putStr ((show a)++" ") 
    (Float a) -> putStr ((show a)++" ") 
    (String a) -> putStr a
    (Bool a) -> putStr ((show a)++" ")  
    --_ -> error "error to print"
    
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