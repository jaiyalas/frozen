{- | Author: 'Jaiyalas' -}

module StinkyTofu.Core.Evaluator where
import StinkyTofu.Core.Syntax
import Data.Bits
import Data.Word

type Env = [(VName,Word64)]
{- evaluator -}

eval :: Env -> Exp -> Word64
eval _ None                  = undefined
eval _ Zero                  = 0
eval _ Unit                  = 1
eval _ (Val w)               = w
eval env (Var vname)           = lk env vname
eval env (If p e0 e1)       
  | eval env p == 0 = eval env e1
  | otherwise       = eval env e0 
eval env (UnOp ope e0)          = case ope of
  Not   -> complement $ eval env e0
  Shl1  -> shiftL (eval env e0) 1
  Shr1  -> shiftR (eval env e0) 1
  Shr4  -> shiftR (eval env e0) 4
  Shr16 -> shiftR (eval env e0) 16 
eval env (BiOp ope e0 e1)       = case ope of
  And  -> (eval env e0) .&. (eval env e1)
  Or   -> (eval env e0) .|. (eval env e1)
  Xor  -> xor (eval env e0) (eval env e1)
  Plus -> (eval env e0) + (eval env e1)
eval env (Fold x xs body) = 
  eval ((VName "y",eval env x):(VName "z",eval env xs):env) body

-- | Lookup
lk :: Env -> VName -> Word64
lk [] s = error ("There is no such thing named "++(show s))
lk ((VName s,n):es) name@(VName t) 
  | s == t    = n
  | otherwise = lk es name
