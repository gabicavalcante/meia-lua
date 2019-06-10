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
                
main :: IO ()
main = case unsafePerformIO (parser (getTokens "1-program.ml")) of
    { 
        Left err -> print err; 
        Right ans -> print ans
    }