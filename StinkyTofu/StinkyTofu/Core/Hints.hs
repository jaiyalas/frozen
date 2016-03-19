module StinkyTofu.Core.Hints where

import StinkyTofu.Core.Syntax

data Hints = 
  HZero  | 
  HUnit  | 
  HIf    | 
  HFold  | 
  HNot   | 
  HShl1  | 
  HShr1  | 
  HShr4  | 
  HShr16 | 
  HAnd   | 
  HOr    | 
  HXor   | 
  HPlus  |
  HVar VName
  deriving (Eq,Show)

-- |'consume' return the consumming amount of a Hints, i.e. 
-- 'consume' = 1 + 'addFork'.

consume :: Hints -> Int
consume HFold = 4 -- 4 = 2 + addFork HFold
consume h = 1 + addFork h

-- |'addFork' the amount of additional branches an operator will provided.

addFork :: Hints -> Int 
addFork HIf   = 2 
addFork HFold = 2 
addFork HAnd  = 1 
addFork HOr   = 1 
addFork HXor  = 1
addFork HPlus = 1
addFork _     = 0

   