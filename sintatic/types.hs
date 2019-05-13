module Types (parser) where
    
import Tokens
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe
import Memory

-- parser to tokens
idToken = tokenPrim show update_pos get_token where
    get_token (Id pos x)    = Just (Id pos x)
    get_token _             = Nothing
 
typeIntToken = tokenPrim show update_pos get_token where
    get_token (IntLit pos x) = Just (IntLit pos x)
    get_token _             = Nothing

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
program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do 
        a <- stmts
        eof
        return (a)

stmts :: ParsecT [Token] [(Token,Token)] IO ([Token])
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

singleStmt :: ParsecT [Token] [(Token,Token)] IO ([Token])
singleStmt = try (
  -- basic (...controle)
  do
   first <- basicStmt
   return (first)
  )

basicStmt :: ParsecT [Token] [(Token,Token)] IO ([Token])
basicStmt = try (
  -- atribuição (...print, chamar procedimento,...)
  do
    first <- assign
    return first
  ) 

assign :: ParsecT [Token] [(Token,Token)] IO ([Token])
assign = do
        a <- idToken
        b <- attribToken
        c <- typeIntToken 
        colon <- semiColonToken 
        updateState(memory_assign (a,c, ))
        s <- getState
        liftIO (print s)
        return (a:b:c:[colon])


-- funções para a tabela de símbolos   

--symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
--symtable_insert symbol []  = [symbol]
--symtable_insert symbol symtable = symtable ++ [symbol]

memory_assign :: String -> Type -> Value -> Scope -> Memory -> Memory
memory_assign id1 type1 value1 scope1 [] = Variable id1 type1 value1 scope1 : []
memory_assign id1 type1 value1 scope1 ((Variable id2 type2 value2 scope2):t) = 
                               if id1 == id2 && scope1 == scope2 then (id2 type2 value1 scope2) : t
                               else (id2 type2 value2 scope2) : memory_assign id1 type1 value1 scope1 t   

memory_remove :: Variable -> Memory -> Memory
memory_remove _ [] = fail "variable not found"
memory_remove (Variable id1 type1 value1 scope1) ((Variable id2 type2 value2 scope2):t) = 
                              if id1 == id2 && scope1 == scope2 then t
                              else (id2 type2 value2 scope2) : memory_remove (id1 type1 value1 scope1) t        

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

-- funções para a tabela de símbolos
                
main :: IO ()
main = case unsafePerformIO (parser (getTokens "1-program.ml")) of
    { 
        Left err -> print err; 
        Right ans -> print ans
    }