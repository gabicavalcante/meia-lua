module TypesNTerminal (parser) where

import Tokens
import Expression
import ParserTokens
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe
import Memory

-- parsers nao terminais
--         ParsecT  input  state       output
program :: ParsecT [Token] Memory IO (ExprTree)
program = do
        a <- stmts  
        eof
        return (UniTree a)

stmts :: ParsecT [Token] Memory IO (ExprTree)
stmts = try (
    do
        a <- basicStmt
        b <- stmts
        return (DualTree a b)
    ) <|> try (
    do
        a <- basicStmt
        return (UniTree a)
    ) 

basicStmt :: ParsecT [Token] Memory IO (ExprTree)
basicStmt = try (
    -- print
    do 
        first <- printToken
        things <- listParam
        colon <- semiColonToken
        return (UniTree things)
    ) <|> try ( 
    do
        first <- assign
        return first
    )

listParam :: ParsecT [Token] Memory IO(ExprTree)
listParam = try (
  -- param, ... , param
  do
    a <- exprNv1
    b <- commaToken
    c <- listParam
    return (DualTree a c) 
  ) <|> (
  -- param
  do 
    a <- exprNv1
    return (UniTree a)
  )

assign :: ParsecT [Token] Memory IO(ExprTree)
assign = do
        a <- idToken
        b <- attribToken
        c <- rightAssign
        colon <- semiColonToken
        return (DualTree (makeToken a) c)

rightAssign :: ParsecT [Token] Memory IO(ExprTree)
rightAssign = try (
    do
        a <- exprNv1
        return a
    ) <|> try (
    do 
        b <- exprAtomic
        return (UniTree b)
    )

scanType :: ParsecT [Token] Memory IO(ExprTree)
scanType = try (
    do
        a <- typeIntToken
        return (AtomicToken a)
    ) <|> try (
    do
        a <- typeFloatToken
        return (AtomicToken a)
    ) <|> try (
    do
        a <- typeStringToken
        return (AtomicToken a)
    ) <|> try (
    do
        a <- typeBooleanToken
        return (AtomicToken a)
    )
    

exprAtomic :: ParsecT [Token] Memory IO(ExprTree) ---ExprTree -> AtomicToken
exprAtomic = try (
    -- StringAtomic
    do
        a <- strLitToken
        return (AtomicToken a)
    ) <|> try (
    -- FloatAtomic
    do
        a <- floatLitToken
        return (AtomicToken a)
    ) <|> try (
    -- IntAtomic
    do
        a <- intToken
        return (AtomicToken a)
    )

-- Expressions
-- Nv1 : + e -
-- Nv2 : * e /
-- Nv3 : ^
-- Nv4 : Parenteshis ( )

exprNv1 :: ParsecT [Token] Memory IO(ExprTree)
exprNv1 = try (
    do
        a <- openParenthToken
        meioParent <- exprNv1
        b <- closeParenthToken
        operator <- operatorNv1
        c <- exprNv1
        return (TriTree meioParent operator c)
    ) <|> try (
    do
        a <- exprNv2
        operator <- operatorNv1
        b <- exprNv1
        return (TriTree a operator b)
    ) <|> (
    do
        a <- exprNv2
        return a
    )

operatorNv1 :: ParsecT [Token] Memory IO(ExprTree)
operatorNv1 = (
    do
        sym <- symOpPlusToken
        return (makeToken sym)
    ) <|> (do
        sym <- symOpMinusToken
        return (makeToken sym)
    )

exprNv2 :: ParsecT [Token] Memory IO(ExprTree)
exprNv2 = try (
    do
        a <- openParenthToken
        meioParent <- exprNv1
        b <- closeParenthToken
        operator <- operatorNv2
        c <- exprNv2
        return (TriTree meioParent operator c)
    ) <|> try (
    do
        a <- exprNv3
        operator <- operatorNv2
        b <- exprNv2
        return (TriTree a operator b)
    ) <|> (
    do
        a <- exprNv3
        return a
    )

operatorNv2 :: ParsecT [Token] Memory IO(ExprTree)
operatorNv2 = (
    do
        sym <- symOpMultToken
        return (makeToken sym)
    ) <|> (do
        sym <- symOpDivToken
        return (makeToken sym)
    )

exprNv3 :: ParsecT [Token] Memory IO(ExprTree)
exprNv3 = try (
    do
        a <- openParenthToken
        meioParent <- exprNv1
        b <- closeParenthToken
        operator <- operatorNv3
        c <- exprNv3
        return (TriTree meioParent operator c)
    ) <|> try (
    do
        a <- exprNv4
        operator <- operatorNv3
        b <- exprNv3
        return (TriTree a operator b)
    ) <|> (
    do
        a <- exprNv4
        return a
    )

operatorNv3 :: ParsecT [Token] Memory IO(ExprTree)
operatorNv3 = (
    do
        sym <- symOpExpToken
        return (makeToken sym)
    )

-- ( )
exprNv4 :: ParsecT [Token] Memory IO(ExprTree)
exprNv4 = try (
-- ( )
    do
        a <- openParenthToken
        meio <- exprNv1
        b <- closeParenthToken
        return meio
    ) <|> try  (
    do
        a <- exprAtomic
        return a
    ) <|> try  (
    do
        a <- idToken
        return (makeToken a)
    ) 

memory_assign :: Variable -> Memory -> Memory
memory_assign symbol (Memory []) = Memory [symbol]
memory_assign (Variable (Id pos1 id1, v1)) (Memory((Variable (Id pos2 id2, v2)) : t)) =
                                if id1 == id2 then append_memory (Variable(Id pos2 id2, v1)) (Memory t)
                                else append_memory (Variable (Id pos2 id2, v2)) (memory_assign (Variable (Id pos1 id1, v1)) (Memory t))

append_memory :: Variable -> Memory -> Memory
append_memory variable (Memory []) = Memory [variable]
append_memory variable (Memory variables) = Memory(variable : variables)

-- Evaluating Expressions
--              memory    arvoreExpr   Memory e valor encontrado
evaluateExpr :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateExpr memory exprTree = case exprTree of
    -- atomics
    AtomicToken a -> case a of
        IntLit _ v -> (memory, (IntType, Int v))
        FloatLit _ v -> (memory, (FloatType, Float v))
        StrLit _ v -> (memory, (StringType, String v))
        SymTrue _ -> (memory, (BoolType, Bool True))
        SymFalse _ -> (memory, (BoolType, Bool False))
    TriTree a b c -> evaluateTriTree memory a b c


evaluateTriTree :: Memory -> ExprTree -> ExprTree -> ExprTree -> (Memory, (Type, Value))
-- Adicao :   a + b
evaluateTriTree memory (AtomicToken (SymOpPlus _)) a b = res
    where
        (mem1, (type1, val1)) = evaluateExpr memory a
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

-- Divisao : a / b
evaluateTriTree mem (AtomicToken (SymOpDiv _)) a b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, exprDiv (type1, val1) (type2, val2))

-- Exponenciacao : a ^ b
triTreeExprParser mem (AtomicToken (SymOpExp _)) a b = res
    where
        (mem1, (type1, val1)) = evaluateExpr mem a
        (mem2, (type2, val2)) = evaluateExpr mem1 b
        res = (mem2, exprExp (type1, val1) (type2, val2))


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


parser :: [Token] -> IO (Either ParseError ExprTree)
parser tokens = runParserT program (Memory []) "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "problem1.ml")) of
    {
        Left err -> print err;
        Right ans -> print ans
    }