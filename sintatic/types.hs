module Types (parser) where
import Text.Parsec

import Tokens
  
-- parser to tokens
idToken = tokenPrim show update_pos get_token where
    get_token (Id pos x)    = Just (Id pos x)
    get_token _             = Nothing
 
typeIntToken = tokenPrim show update_pos get_token where
    get_token (TypeInt pos) = Just (TypeInt pos)
    get_token _             = Nothing

attribToken = tokenPrim show update_pos get_token where
    get_token (Attrib pos) = Just (Attrib pos)
    get_token _            = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos 
update_pos pos _ []      = pos 


-- parsers nao terminais
program :: Parsec [Token] st [Token]
program = do 
        a <- idToken 
        b <- stmts
        eof
        return (a:b)

stmts :: Parsec [Token] st [Token]
stmts = do
        first <- assign
        return (first)

assign :: Parsec [Token] st [Token]
assign = do
        a <- idToken
        b <- attribToken
        c <- typeIntToken
        return (a:b:[c])

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens
 
main :: IO ()
main = case parser (getTokens "1-program.ml") of
    { Left err -> print err; 
        Right ans -> print ans
    }