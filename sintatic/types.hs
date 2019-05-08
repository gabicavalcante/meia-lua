module Types (parser) where

import Tokens
import Text.Parsec

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
 
semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon pos) = Just (SemiColon pos)
  get_token _         = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos 
update_pos pos _ []      = pos 


-- parsers nao terminais
--         Parsec  input       state       output
program :: Parsec [Token] [(Token,Token)] [Token]
program = do 
        a <- stmts
        eof
        return (a)

stmts :: Parsec [Token] [(Token,Token)] [Token]
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

singleStmt :: Parsec [Token] [(Token,Token)] [Token]
singleStmt = try (
  -- basic (...controle)
  do
   first <- basicStmt
   return (first)
  )

basicStmt :: Parsec [Token] [(Token,Token)] [Token]
basicStmt = try (
  -- atribuição (...print, chamar procedimento,...)
  do
    first <- assign
    return first
  ) 

assign :: Parsec [Token] [(Token,Token)] [Token]
assign = do
        a <- idToken
        b <- attribToken
        c <- typeIntToken 
        colon <- semiColonToken
        return (a:b:c:[colon])

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program [] "Error message" tokens
 
main :: IO ()
main = case parser (getTokens "1-program.ml") of
    { 
        Left err -> print err; 
        Right ans -> print ans
    }