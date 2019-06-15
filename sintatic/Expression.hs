module Expression where

import Tokens

data ExprTree = TriTree ExprTree ExprTree ExprTree |
                DualTree ExprTree ExprTree |
                UniTree ExprTree |
                None |
                AtomicToken Token deriving (Eq, Show)