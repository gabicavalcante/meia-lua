module Memory where
import Tokens

-- (nome e profundidade do subbloco)
data Scope = Scope (String, Int) deriving (Eq, Show)

data Memory = Memory [Variable] deriving (Eq, Show)
--                        id    tipo valor escopo
--data Variable = Variable String Type Value Scope deriving (Eq, Show)
data Variable = Variable (Token, Token) deriving (Eq, Show)

data Type = IntType | FloatType | StringType | BoolType deriving (Show)

instance Eq Type where
    (IntType) == (IntType) = True
    (FloatType) == (FloatType) = True
    (StringType) == (StringType) = True
    (BoolType) == (BoolType) = True

data Value = Int Int |
    Float Float |
    String String |
    Bool Bool deriving (Eq, Show)
 
                
data ExprTree = TriTree ExprTree ExprTree ExprTree |
                AtomicToken Token deriving (Eq, Show)