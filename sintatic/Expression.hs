module Expression where

import Tokens

data ExprTree = TripleNode NonTToken ExprTree ExprTree ExprTree |
                DoubleNode NonTToken ExprTree ExprTree |
                SingleNode NonTToken ExprTree |
                None |
                AtomicToken Token deriving (Eq, Show)

data NonTToken =  NonTProgram           |
                NonTStatements          |
                NonTStatement           |   
                NonTAssign              | 
                NonTIf                  |
                NonTElse                |
                NonTWhile               |
                NonTExpr                | 
                NonTId                  |  
                NonTParams              |
                NonTListIndex           |
                NonTIndex               |
                NonTDecl                |
                NonTPrint               |   
                NonTStructType deriving (Eq, Show)