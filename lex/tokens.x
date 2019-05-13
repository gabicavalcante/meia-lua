{
module Tokens (getTokens, Token(..), AlexPosn(..), alexScanTokens) where
--module Main (main, Token(..), AlexPosn(..), alexScanTokens) where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-

  $white+                                     ;
  "--".*.                                     ;
  int                                         { \p s -> TypeInt p }
  float                                       { \p s -> TypeFloat p }
  string                                      { \p s -> TypeString p } 
  bool                                        { \p s -> TypeBoolean p } 
  $digit+\.$digit+                            { \p s -> FloatLit p (read s) }
  $digit+                                     { \p s -> IntLit p (read s) }
  \:                                          { \p s -> Colon p }
  \;                                          { \p s -> SemiColon p }
  \,                                          { \p s -> Comma p }
  \.                                          { \p s -> EndPoint p }
  \=\=                                        { \p s -> SymEq p }
  \!\=                                        { \p s -> SymNotEq p }
  \<\=                                        { \p s -> SymLessThanEq p }
  \>\=                                        { \p s -> SymGreaterThanEq p }
  \!                                          { \p s -> SymNot p }
  \&\&                                        { \p s -> SymAnd p }
  \|\|                                        { \p s -> SymOr p }
  true                                        { \p s -> SymTrue p }
  false                                       { \p s -> SymFalse p }
  \<                                          { \p s -> SymLessThan p }
  \>                                          { \p s -> SymGreaterThan p }
  \+                                          { \p s -> SymOpPlus p }
  \-                                          { \p s -> SymOpMinus p }
  \*                                          { \p s -> SymOpMult p }
  \/                                          { \p s -> SymOpDiv p }
  \^                                          { \p s -> SymOpExp p }
  \%                                          { \p s -> SymOpMod p }
  \+\+                                        { \p s -> SymOpPlusPlus p }
  \-\-                                        { \p s -> SymOpMinusMinus p }
  \+\=                                        { \p s -> SymOpPlusAssign p }
  \-\=                                        { \p s -> SymOpMinusAssign p }
  \*\=                                        { \p s -> SymOpMultAssign p }
  \/\=                                        { \p s -> SymOpDivAssign p }
  \=                                          { \p s -> Attrib p }
  \(                                          { \p s -> OpenParenth p }
  \)                                          { \p s -> CloseParenth p }
  \[                                          { \p s -> OpenBracket p }
  \]                                          { \p s -> CloseBracket p }
  \{                                          { \p s -> OpenScope p }
  \}                                          { \p s -> CloseScope p }
  \=\>                                        { \p s -> SymPtrOp p }
  \$                                          { \p s -> SymAdressOp p }
  print                                       { \p s -> Print p }
  scan                                        { \p s -> Scan p }
  do                                          { \p s -> Do p } 
  for                                         { \p s -> For p }    
  while                                       { \p s -> While p } 
  if                                          { \p s -> If p }
  then                                        { \p s -> Then p } 
  else                                        { \p s -> Else p } 
  elif                                        { \p s -> Elif p }
  procedure                                   { \p s -> Procedure p }
  function                                    { \p s -> Function p }
  return                                      { \p s -> Return p }
  break                                       { \p s -> Break p }
  continue                                    { \p s -> Continue p }  
  table                                       { \p s -> Table p } 
  $alpha [$alpha $digit \_ \']*               { \p s -> Id p s }
  \" [^\"]* \"       { \p s -> StrLit p (firstLast s) }
{
firstLast :: [a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeInt AlexPosn                |
  TypeFloat AlexPosn              |
  TypeString AlexPosn             | 
  TypeBoolean AlexPosn            |
  Attrib AlexPosn                 |
  OpenParenth AlexPosn            |
  CloseParenth AlexPosn           |
  OpenBracket AlexPosn            |
  CloseBracket AlexPosn           |
  OpenScope AlexPosn              |
  CloseScope AlexPosn             |
  Colon AlexPosn                  |
  SemiColon AlexPosn              |
  Comma AlexPosn                  |
  EndPoint AlexPosn               |
  SymPtrOp AlexPosn               |
  SymAdressOp AlexPosn            |
  Print AlexPosn                  |
  Scan AlexPosn                   |
  Do AlexPosn                     |
  If AlexPosn                     | 
  Then AlexPosn                   |
  Else AlexPosn                   | 
  Elif AlexPosn                   | 
  For AlexPosn                    |  
  While AlexPosn                  | 
  Procedure AlexPosn              |
  Function AlexPosn               |
  Return AlexPosn                 |
  Break AlexPosn                  |
  Continue AlexPosn               |  
  Table AlexPosn                  |
  FloatLit AlexPosn Float         |
  IntLit AlexPosn Int             |
  StrLit AlexPosn String          |
  SymOpPlus AlexPosn              |
  SymOpMinus AlexPosn             |
  SymOpMult AlexPosn              |
  SymOpDiv AlexPosn               |
  SymOpExp AlexPosn               |
  SymOpMod AlexPosn               |
  SymOpPlusPlus AlexPosn          |
  SymOpMinusMinus AlexPosn        |
  SymOpPlusAssign AlexPosn        |
  SymOpMinusAssign AlexPosn       |
  SymOpMultAssign AlexPosn        |
  SymOpDivAssign AlexPosn         |
  SymEq AlexPosn                  |
  SymNotEq AlexPosn               |
  SymLessThanEq AlexPosn          |
  SymGreaterThanEq AlexPosn       |
  SymNot AlexPosn                 |
  SymAnd AlexPosn                 |
  SymOr AlexPosn                  |
  SymTrue AlexPosn                |
  SymFalse AlexPosn               |
  SymLessThan AlexPosn            |
  SymGreaterThan AlexPosn         | 
  Id AlexPosn String    
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)
getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
--main = do
--  s <- getContents
--  print (alexScanTokens s)
--}

