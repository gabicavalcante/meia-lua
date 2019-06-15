module Expression where

import Tokens

data ExprTree = TripleNode ExprTree ExprTree ExprTree |
                DoubleNode ExprTree ExprTree |
                SingleNode ExprTree |
                None |
                AtomicToken Token deriving (Eq, Show)