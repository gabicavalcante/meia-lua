{
module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  int                                  { \p s -> TypeInt p }
  float                                { \p s -> TypeFloat p }
  string                               { \p s -> TypeString p }
  bool                                 { \p s -> TypeBoolean p } 
  true                                 { \p s -> SymBoolTrue p }
  false                                { \p s -> SymBoolFalse p } 
  \<                                   { \p s -> SymBoolLessThan p }
  \>                                   { \p s -> SymBoolGreaterThan p }
  \+                                   { \p s -> SymOpPlus p }  
  \(                                   { \p s -> OpenParenth p }
  \)                                   { \p s -> CloseParenth p }
  \{                                   { \p s -> OpenScope p }
  \}                                   { \p s -> CloseScope p }  
  while                                { \p s -> While p }
  =                                    { \s -> Assign} 
  print                                { \s -> Print} 
  $digit+                              { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  TypeInt AlexPosn Int            |
  TypeFloat AlexPosn              |
  TypeString AlexPosn             | 
  TypeBoolean AlexPosn            |  
  OpenParenth AlexPosn            |
  CloseParenth AlexPosn           | 
  OpenScope AlexPosn              |
  CloseScope AlexPosn             |   
  Print AlexPosn                  |   
  While AlexPosn                  |   
  SymOpPlus AlexPosn              |  
  SymBoolTrue AlexPosn            |
  SymBoolFalse AlexPosn           |
  SymBoolLessThan AlexPosn        |
  SymBoolGreaterThan AlexPosn     | 
  Id AlexPosn String    
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}