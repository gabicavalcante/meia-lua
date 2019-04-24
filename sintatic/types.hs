module Types (TokenTree(..), NonTToken(..), parser) where
import Text.Parsec

import Tokens

data TokenTree = QuadTree NonTToken TokenTree TokenTree TokenTree TokenTree |
                 TriTree NonTToken TokenTree TokenTree TokenTree |
                 DualTree NonTToken TokenTree TokenTree |
                 UniTree NonTToken TokenTree |
                 None |
                 LeafToken Token deriving (Eq, Show)

data NonTToken = 
    NonTProgram |
    NonTStatements |
    NonTStatement |       
    NonTDecl               |   
    NonTListType           |
    NonTStructType deriving (Eq, Show)

makeToken :: Token -> TokenTree
makeToken tok = LeafToken tok

-- parser to tokens
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


update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos 
update_pos pos _ []      = pos 


-- parsers nao terminais
--         Parsec   input       state         output
program :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
program =
    do c <- stmts
        eof
        return (UniTree NonTProgram c)
 

stmts :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
stmts = try (
    do
        a <- singleStmt
        b <- stmts
        return (DualTree NonTStatements a b)
    ) <|> try (
    do
        a <- singleStmt
        return (UniTree NonTStatement a)
    )

singleStmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
singleStmt =  
    -- Basico
    do
    first <- basicStmt
    colon <- semicolonToken
    return first

    
basicStmt :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
basicStmt = try (
    -- Declarações
    do
        first <- decl
        return first
    ) 

decl :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
decl =  
    do 
        type_token <- types
        id <- listIds
        return ( DualTree NonTDecl type_token id)  
 
    

types :: ParsecT [Token] [(Token,Token)] IO(TokenTree)
types = try (
    do
        t <- openBracketToken
        t2 <- types
        t3 <- closeBracketToken
        return (UniTree NonTListType t2)
    ) <|> try (
    do
        t <- typeIntToken
        return (LeafToken t)
    ) <|> try (
    do 
        t <- typeFloatToken
        return (LeafToken t)
    ) <|> try (
    do 
        t <- typeStringToken
        return (LeafToken t)
    ) <|> try (
    do
        t <- typeBooleanToken
        return (LeafToken t)
    ) <|> (
    do
        id <- idToken
        return (UniTree NonTStructType (makeToken id))
    )




parser :: [Token] -> IO ()
parser tokens = runParserT program [] "Error message" tokens