module Types (parser) where

import Tokens
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe
import Memory

-- paerser to types
typeIntToken :: ParsecT [Token] st IO (Token)
typeIntToken = tokenPrim show update_pos get_token where
    get_token (TypeInt pos) = Just (TypeInt pos)
    get_token _             = Nothing

typeFloatToken :: ParsecT [Token] st IO (Token)
typeFloatToken = tokenPrim show update_pos get_token where
    get_token (TypeFloat pos)   = Just (TypeFloat pos)
    get_token _                 = Nothing

typeStringToken :: ParsecT [Token] st IO (Token)
typeStringToken = tokenPrim show update_pos get_token where
    get_token (TypeString pos)    = Just (TypeString pos)
    get_token _                   = Nothing

typeBooleanToken :: ParsecT [Token] st IO (Token)
typeBooleanToken = tokenPrim show update_pos get_token where
    get_token (TypeBoolean pos) = Just (TypeBoolean pos)
    get_token _                 = Nothing

-- parser to tokens
idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
    get_token (Id pos x)    = Just (Id pos x)
    get_token _             = Nothing

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
    get_token (IntLit pos x) = Just (IntLit pos x)
    get_token _             = Nothing

floatLitToken :: ParsecT [Token] st IO (Token)
floatLitToken = tokenPrim show update_pos get_token where
  get_token (FloatLit pos x)    = Just (FloatLit pos x)
  get_token _                 = Nothing

strLitToken :: ParsecT [Token] st IO (Token)
strLitToken = tokenPrim show update_pos get_token where
  get_token (StrLit pos x) = Just (StrLit pos x)
  get_token _              = Nothing

attribToken :: ParsecT [Token] st IO (Token)
attribToken = tokenPrim show update_pos get_token where
    get_token (Attrib pos) = Just (Attrib pos)
    get_token _            = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon pos) = Just (SemiColon pos)
  get_token _         = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos
update_pos pos _ []      = pos


-- parsers nao terminais
--         ParsecT  input       state       output
program :: ParsecT [Token] Memory IO ([Token])
program = do
        a <- stmts
        eof
        return (a)

stmts :: ParsecT [Token] Memory IO ([Token])
stmts = try (
  do
    a <- singleStmt
    b <- stmts
    return (a ++ b)
  ) <|> try (
  do
    a <- singleStmt
    return (a)
  )

singleStmt :: ParsecT [Token] Memory IO ([Token])
singleStmt = try (
  -- basic (...controle)
  do
   first <- basicStmt
   return (first)
  )

basicStmt :: ParsecT [Token] Memory IO ([Token])
basicStmt = try (
  -- atribuição (...print, chamar procedimento,...)
  do
    first <- assign
    return first
  )

assign :: ParsecT [Token] Memory IO ([Token])
assign = try (
    do
      a <- idToken
      b <- attribToken
      c <- intToken
      colon <- semiColonToken
      updateState(memory_assign (Variable(a, c)))
      s <- getState
      liftIO (print s)
      return (a:b:c:[colon])
  ) <|> try (
    do
      a <- idToken
      b <- attribToken
      c <- floatLitToken
      colon <- semiColonToken
      updateState(memory_assign (Variable(a, c)))
      s <- getState
      liftIO (print s)
      return (a:b:c:[colon])
  ) <|> try (
    do
      a <- idToken
      b <- attribToken
      c <- strLitToken
      colon <- semiColonToken
      updateState(memory_assign (Variable(a, c)))
      s <- getState
      liftIO (print s)
      return (a:b:c:[colon])
  )

memory_assign :: Variable -> Memory -> Memory
memory_assign symbol (Memory []) = Memory [symbol]
memory_assign (Variable (Id pos1 id1, v1)) (Memory((Variable (Id pos2 id2, v2)) : t)) =
                              if id1 == id2 then append_memory (Variable(Id pos2 id2, v1)) (Memory t)
                              else append_memory (Variable (Id pos2 id2, v2)) (memory_assign (Variable (Id pos1 id1, v1)) (Memory t))

append_memory :: Variable -> Memory -> Memory
append_memory variable (Memory []) = Memory [variable]
append_memory variable (Memory variables) = Memory(variable : variables)

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (Memory []) "Error message" tokens

-- funções para a tabela de símbolos

-- Operation Symbols

symOpPlusToken :: ParsecT [Token] st IO (Token)
symOpPlusToken = tokenPrim show update_pos get_token where
  get_token (SymOpPlus pos) = Just (SymOpPlus pos)
  get_token _               = Nothing

symOpMinusToken :: ParsecT [Token] st IO (Token)
symOpMinusToken = tokenPrim show update_pos get_token where
  get_token (SymOpMinus pos) = Just (SymOpMinus pos)
  get_token _                = Nothing

symOpMultToken :: ParsecT [Token] st IO (Token)
symOpMultToken = tokenPrim show update_pos get_token where
  get_token (SymOpMult pos) = Just (SymOpMult pos)
  get_token _               = Nothing

symOpDivToken :: ParsecT [Token] st IO (Token)
symOpDivToken = tokenPrim show update_pos get_token where
  get_token (SymOpDiv pos)  = Just (SymOpDiv pos)
  get_token _               = Nothing

symOpExpToken :: ParsecT [Token] st IO (Token)
symOpExpToken = tokenPrim show update_pos get_token where
  get_token (SymOpExp pos)  = Just (SymOpExp pos)
  get_token _               = Nothing

-- Brackets

openParenthToken :: ParsecT [Token] st IO (Token)
openParenthToken = tokenPrim show update_pos get_token where
  get_token (OpenParenth pos) = Just (OpenParenth pos)
  get_token _                 = Nothing

closeParenthToken :: ParsecT [Token] st IO (Token)
closeParenthToken = tokenPrim show update_pos get_token where
  get_token (CloseParenth pos) = Just (CloseParenth pos)
  get_token _                  = Nothing

openBracketToken :: ParsecT [Token] st IO (Token)
openBracketToken = tokenPrim show update_pos get_token where
  get_token (OpenBracket pos) = Just (OpenBracket pos)
  get_token _                 = Nothing

closeBracketToken :: ParsecT [Token] st IO (Token)
closeBracketToken = tokenPrim show update_pos get_token where
  get_token (CloseBracket pos) = Just (CloseBracket pos)
  get_token _                  = Nothing

openScopeToken :: ParsecT [Token] st IO (Token)
openScopeToken = tokenPrim show update_pos get_token where
  get_token (OpenScope pos) = Just (OpenScope pos)
  get_token _               = Nothing

closeScopeToken :: ParsecT [Token] st IO (Token)
closeScopeToken = tokenPrim show update_pos get_token where
  get_token (CloseScope pos) = Just (CloseScope pos)
  get_token _                = Nothing


makeToken :: Token -> TokenTree
makeToken tok = AtomicToken tok

-- Evaluating Expressions
--                 memory    arvoreExpr   Memory e valor encontrado
evaluateExpr :: Memory -> ExprTree -> (Memory, (Type, Value))
evaluateExpr memory exprTree = case tree of
    -- atomics
    AtomicToken a -> case a of
        IntLit _ v -> (memory, (IntType, Int v))
        FloatLit _ v -> (memory, (FloatType, Float v))
        StrLit _ v -> (memory, (StringType, String v))
        SymBoolTrue _ -> (memory, (BoolType, Bool True))
        SymBoolFalse _ -> (memory, (BoolType, Bool False))
    TriTree a b c -> evaluateTriTree st a b c


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
triTreeExprParser mem (LeafToken (SymOpExp _)) a b = res
where
    (mem1, (type1, val1)) = evaluateExpr mem a
    (mem2, (type2, val2)) = evaluateExpr mem1 b
    res = (mem2, exprExp (type1, val1) (type2, val2))


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

exprAtomic :: Expression -> Value
  exprFinalIds = try (
    -- StringAtomic
    do
      a <- strLitToken
      return (Value a)
    ) <|> try (
    -- FloatAtomic
    do
      a <- floatLitToken
      return (Value a)
    ) <|> try (
    -- IntAtomic
    do
      a <- intLitToken
      return (Value a)
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
      operator <- OperatorNv1
      c <- exprNv1
      return (TriTree Memory meioParent operator c)
    ) <|> try (
      do
        a <- exprNv2
        operator <- OperatorNv1
        b <- exprNv1
        return (TriTree NonTExpr a operator b)
    ) <|> (
      do
        a <- exprNv2
        return a
    )

  OperatorNv1 :: ParsecT [Token] Memory IO(TokenTree)
  OperatorNv1 = (
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
      operator <- OperatorNv2
      c <- exprNv2
      return (TriTree Memory meioParent operator c)
    ) <|> try (
      do
        a <- exprNv3
        operator <- OperatorNv2
        b <- exprNv2
        return (TriTree NonTExpr a operator b)
    ) <|> (
      do
        a <- exprNv3
        return a
    )

  OperatorNv2 :: ParsecT [Token] Memory IO(TokenTree)
  OperatorNv2 = (
    do
      sym <- symOpPlusToken
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
      operator <- OperatorNv3
      c <- exprNv3
      return (TriTree Memory meioParent operator c)
    ) <|> try (
      do
        a <- exprNv4
        operator <- OperatorNv3
        b <- exprNv3
        return (TriTree NonTExpr a operator b)
    ) <|> (
      do
        a <- exprNv4
        return a
    )

  OperatorNv3 :: ParsecT [Token] Memory IO(TokenTree)
  OperatorNv3 = (
    do
      sym <- symOpExpToken
      return (makeToken sym)
    )


  exprNv4 :: ParsecT [Token] Memory IO(TokenTree)
  exprNv4 = try (
  -- ( )
  do
    a <- openParenthToken
    meio <- exprNv1
    b <- closeParenthToken
    return meio
  )

-- +-
expreSumSub :: ParsecT [Token] Memory IO(ExprTree)
expreSumSub = (
  do
    sym <- symOpPlusToken
    return (makeToken sym)
  ) <|> (do
    sym <- symOpMinusToken
    return (makeToken sym)
  )

-- * /
expreMultDiv :: ParsecT [Token] Memory IO(ExprTree)
expreMultDiv = (
  do
    sym <- symOpMultToken
    return (makeToken sym)
  ) <|> (do
    sym <- symOpDivToken
    return (makeToken sym)
  ) <|> (do
    sym <- symOpModToken
    return (makeToken sym)
  )

main :: IO ()
main = case unsafePerformIO (parser (getTokens "1-program.ml")) of
    {
        Left err -> print err;
        Right ans -> print ans
    }