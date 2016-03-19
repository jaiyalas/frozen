{- | Author: 'Jaiyalas' -}

module StinkyTofu.Search.Simple where

import StinkyTofu.Core.Syntax
import StinkyTofu.Core.Hints as Hint

import Data.List
import Data.Word

type Examples = [(Word64,Word64)]

allOps :: Int -> [Hints] -> [[Hints]]
allOps n hints = 
  let -- size reduction
    nodes  = (length hints)
    forks = 1 + (foldr (\x hs -> (addFork x) + hs) 0 hints)
    s = n - (nodes+forks)
    -- fold detection
    (comp,hints') = if (elem HFold hints)
      then (\x -> HFold : x,  filter (/= HFold) hints) 
      else (id, hints)
  in map (comp . (hints++)) $ additional s hints'

additional :: Int -> [Hints] -> [[Hints]]
additional n = 
  let p = \x -> (sum $ map consume x) == n
      concatPlus [] = []
      concatPlus ([]:xs) = concatPlus xs
      concatPlus ((y:ys):xs) = y : (concatPlus (ys:xs))
  in filter p . concatPlus . map inits . tails

allLeaf :: Bool -> Int -> [Exp]
allLeaf exFold _ = if exFold 
  then [Zero,Unit,l "x",l "y",l "z"]
  else [Zero,Unit,l "x"]
    
-- allNodes n hints = (allOps n hints)


-- l2t :: [Hints] -> [Exp] -> Exp
-- l2t [] = error "Yo~"
-- l2t (x:xs) = case x of 
-- l2t (x:xs) = case x of 
--   HZero  env -> 
--   HUnit  env -> 
--   HNot   env -> 
--   HShl1  env -> 
--   HShr1  env -> 
--   HShr4  env -> 
--   HShr16 env -> 
--   -- 
--   HAnd   env -> 
--   HOr    env -> 
--   HXor   env -> 
--   HPlus  env -> 
--   HIf    env -> 
--   -- 
--   HFold  env -> 
--   HVar v env -> (Var v):env
    