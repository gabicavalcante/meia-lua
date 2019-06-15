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
        id <- idToken 
        return (makeToken id)
    ) 

memory_assign :: Variable -> Memory -> Memory
memory_assign symbol (Memory []) = Memory [symbol]
memory_assign (Variable (Id pos1 id1, v1)) (Memory((Variable (Id pos2 id2, v2)) : t)) =
                                if id1 == id2 then append_memory (Variable(Id pos2 id2, v1)) (Memory t)
                                else append_memory (Variable (Id pos2 id2, v2)) (memory_assign (Variable (Id pos1 id1, v1)) (Memory t))

append_memory :: Variable -> Memory -> Memory
append_memory variable (Memory []) = Memory [variable]
append_memory variable (Memory variables) = Memory(variable : variables)

parser :: [Token] -> IO (Either ParseError ExprTree)
parser tokens = runParserT program (Memory []) "Error message" tokens

--main :: IO ()
--main = case unsafePerformIO (parser (getTokens "problem1.ml")) of
--    {
--        Left err -> print err;
--        Right ans -> print ans
--    }