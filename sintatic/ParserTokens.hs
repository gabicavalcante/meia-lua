module ParserTokens where

import Tokens
import Expression
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

symTrueToken :: ParsecT [Token] st IO (Token)
symTrueToken = tokenPrim show update_pos get_token where
  get_token (SymTrue pos) = Just (SymTrue pos)
  get_token _                = Nothing

symFalseToken :: ParsecT [Token] st IO (Token)
symFalseToken = tokenPrim show update_pos get_token where
  get_token (SymFalse pos) = Just (SymFalse pos)
  get_token _                = Nothing


-- parser to tokens
idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
    get_token (Id pos x)    = Just (Id pos x)
    get_token _             = Nothing

intLitToken :: ParsecT [Token] st IO (Token)
intLitToken = tokenPrim show update_pos get_token where
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

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma pos) = Just (Comma pos)
  get_token _               = Nothing

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

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print pos)  = Just (Print pos)
  get_token _            = Nothing


update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos
update_pos pos _ []      = pos

makeToken :: Token -> ExprTree
makeToken tok = AtomicToken tok