--  (nome e profundidade do subprograma) (nome e profundidade do subbloco)
data Scope = Scope (String, Int) (String, Int) deriving (Eq, Show)

import Tokens

data Memory = Memory [(Token,Token)] deriving (Eq, Show)

