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

symBoolNotToken :: ParsecT [Token] st IO (Token)
symBoolNotToken = tokenPrim show update_pos get_token where
  get_token (SymNot pos) = Just (SymNot pos)
  get_token _                = Nothing

symBoolAndToken :: ParsecT [Token] st IO (Token)
symBoolAndToken = tokenPrim show update_pos get_token where
  get_token (SymAnd pos) = Just (SymAnd pos)
  get_token _                = Nothing

symBoolOrToken :: ParsecT [Token] st IO (Token)
symBoolOrToken = tokenPrim show update_pos get_token where
  get_token (SymOr pos) = Just (SymOr pos)
  get_token _               = Nothing

symBoolTrueToken :: ParsecT [Token] st IO (Token)
symBoolTrueToken = tokenPrim show update_pos get_token where
  get_token (SymTrue pos) = Just (SymTrue pos)
  get_token _                = Nothing

symBoolFalseToken :: ParsecT [Token] st IO (Token)
symBoolFalseToken = tokenPrim show update_pos get_token where
  get_token (SymFalse pos) = Just (SymFalse pos)
  get_token _                = Nothing

symBoolEqToken :: ParsecT [Token] st IO (Token)
symBoolEqToken = tokenPrim show update_pos get_token where
  get_token (SymEq pos)   = Just (SymEq pos)
  get_token _                 = Nothing

symBoolNotEqToken :: ParsecT [Token] st IO (Token)
symBoolNotEqToken = tokenPrim show update_pos get_token where
  get_token (SymNotEq pos) = Just (SymNotEq pos)
  get_token _                  = Nothing

symBoolLessThanEqToken :: ParsecT [Token] st IO (Token)
symBoolLessThanEqToken = tokenPrim show update_pos get_token where
  get_token (SymLessThanEq pos) = Just (SymLessThanEq pos)
  get_token _                       = Nothing

symBoolGreaterThanEqToken :: ParsecT [Token] st IO (Token)
symBoolGreaterThanEqToken = tokenPrim show update_pos get_token where
  get_token (SymGreaterThanEq pos) = Just (SymGreaterThanEq pos)
  get_token _                          = Nothing

symBoolLessThanToken :: ParsecT [Token] st IO (Token)
symBoolLessThanToken = tokenPrim show update_pos get_token where
  get_token (SymLessThan pos) = Just (SymLessThan pos)
  get_token _                     = Nothing

symBoolGreaterThanToken :: ParsecT [Token] st IO (Token)
symBoolGreaterThanToken = tokenPrim show update_pos get_token where
  get_token (SymGreaterThan pos) = Just (SymGreaterThan pos)
  get_token _                        = Nothing

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

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print pos)  = Just (Print pos)
  get_token _            = Nothing

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

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma pos) = Just (Comma pos)
  get_token _               = Nothing

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
    first <- printToken
    things <- listParam
    return things
  ) <|> try (
  do
    first <- assign
    return first
  ) 

listParam :: ParsecT [Token] Memory IO ([Token])
listParam = try (
  -- param, ... , param
  do
    a <- expr0
    b <- commaToken
    c <- listParam
    return a c 
  ) <|> (
  -- param
  do 
    a <- expr0
    return a
  )

-- &&  ||
expr0 :: ParsecT [Token] Memory IO ([Token])
expr0 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr0Ops
    c <- expr0
    return (meio c)
  ) <|> try (
  do
    a <- expr1
    meio <- expr0Ops
    b <- expr0
    return (a meio b)
  ) <|> try (
  do
    a <- expr1
    return a
  )

expr0Ops :: ParsecT [Token] Memory IO ([Token])
expr0Ops = 
  (do 
    sym <- symBoolAndToken
    return (sym)
  ) <|> (do
    sym <- symBoolOrToken
    return (sym)
  )

-- !
expr1 :: ParsecT [Token] Memory IO ([Token])
expr1 = try (
  do
    meio <- expr1Ops
    c <- expr1
    return (meio c)
  ) <|> try (
  do
    a <- expr2
    return a
  )

expr1Ops :: ParsecT [Token] Memory IO ([Token])
expr1Ops = 
  (do
    sym <- symBoolNotToken
    return (sym)
  )

-- <  >  <=  >=  ==  !=
expr2 :: ParsecT [Token] Memory IO ([Token])
expr2 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr2Ops
    c <- expr2
    return (meioParent meio c)
  ) <|> try (
  do
    a <- expr3
    meio <- expr2Ops
    b <- expr2
    return (a meio b)
  ) <|> (
  do
    a <- expr3
    return a
  )

expr2Ops :: ParsecT [Token] Memory IO ([Token])
expr2Ops = 
  (do
    sym <- symBoolLessThanToken
    return (sym)
  ) <|> (do
    sym <- symBoolGreaterThanToken
    return (sym)
  ) <|> (do
    sym <- symBoolLessThanEqToken
    return (sym)
  ) <|> (do
    sym <- symBoolGreaterThanEqToken
    return (sym)
  ) <|> (do
    sym <- symBoolEqToken
    return (sym)
  ) <|> (do
    sym <- symBoolNotEqToken
    return (sym)
  )

-- +  -
expr3 :: ParsecT [Token] Memory IO ([Token])
expr3 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr3Ops
    c <- expr3
    return (meioParent meio c)
  ) <|> try (
  do
    a <- expr4
    meio <- expr3Ops
    b <- expr3
    return (a meio b)
  ) <|> (
  do
    a <- expr4
    return a
  )

expr3Ops :: ParsecT [Token] Memory IO ([Token])
expr3Ops = (
  do
    sym <- symOpPlusToken
    return (sym)
  ) <|> (do
    sym <- symOpMinusToken
    return (sym)
  )

-- *  /  %
expr4 :: ParsecT [Token] Memory IO ([Token])
expr4 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr4Ops
    c <- expr4
    return (meioParent meio c)
  ) <|> try (
  do
    a <- expr5
    meio <- expr4Ops
    b <- expr4
    return (a meio b)
  ) <|> (
  do
    a <- expr5
    return a
  )
 
expr4Ops :: ParsecT [Token] Memory IO ([Token])
expr4Ops = (
  do
    sym <- symOpMultToken
    return (sym)
  ) <|> (do
    sym <- symOpDivToken
    return (sym)
  ) 

-- ^
expr5 :: ParsecT [Token] Memory IO ([Token])
expr5 = try (
  do
    a <- openParenthToken
    meioParent <- expr0
    b <- closeParenthToken
    meio <- expr5Ops
    c <- expr5
    return (meioParent meio c)
  ) <|> try (
  do
    a <- exprParenth
    meio <- expr5Ops
    b <- expr5
    return (a meio b)
  ) <|> (
  do
    a <- exprParenth
    return a
  )

expr5Ops :: ParsecT [Token] Memory IO ([Token])
expr5Ops = (
  do
    sym <- symOpExpToken
    return (sym)
  )

-- ( )
exprParenth :: ParsecT [Token] Memory IO ([Token])
exprParenth = try (
  -- ( )
  do
    a <- openParenthToken
    meio <- expr0
    b <- closeParenthToken
    return meio
  ) <|> (
    do
      a <- exprFinalIds
      return a
  ) 

-- function, ids e literis
exprFinalIds :: ParsecT [Token] Memory IO ([Token])
exprFinalIds = try (
  -- function
  do
    a <- exprFunction
    return a
  ) <|> try (
  -- true lit
  do 
    a <- symBoolTrueToken
    return (a)
  ) <|> (
  -- false lit
  do
    a <- symBoolFalseToken
    return (a)
  ) <|> try (
  -- id
  do
    a <- idToken
    return (a)
  ) <|> try (
  -- strlit
  do
    a <- strLitToken
    return (a)
  ) <|> try (
  -- floatlit
  do
    a <- floatLitToken
    return (a)
  ) <|> try (
  -- intlit
  do
    a <- intToken
    return (a)
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

  exprFunction :: ParsecT [Token] Memory IO ([Token])
  exprFunction = try (
    -- a(3, 4, ...)
    do 
      name <- idToken
      a <- openParenthToken
      b <- listParam
      c <- closeParenthToken
      return (name:a:b:[c]) -- ?
    ) <|> (
    -- a()
    do
      name <- idToken
      a <- openParenthToken
      b <- closeParenthToken
      return (name:a:[b]) -- ?
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
                
main :: IO ()
main = case unsafePerformIO (parser (getTokens "problem1.ml")) of
    { 
        Left err -> print err; 
        Right ans -> print ans
    }