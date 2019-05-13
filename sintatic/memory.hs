module Memory where

import Tokens

data Memory = Memory [(Token,Token)] deriving (Eq, Show)