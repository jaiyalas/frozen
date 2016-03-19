{- | Author: 'Jaiyalas' -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module StinkyTofu.Core.Syntax where

import Data.Word
import Data.List

{- Primitive Structure-}
newtype VName = VName {getName :: String} deriving Eq
data Prog = Lambda {getBinder :: Exp, getBody ::Exp}
data Exp = None | Zero | Unit | Var VName | Val Word64
  | If   {pred :: Exp, branch_T :: Exp, branch_F :: Exp}
  | Fold Exp Exp Exp
  | UnOp {unop :: Op1, unOpd :: Exp}
  | BiOp {biop :: Op2, biOpdL :: Exp, biOpdR :: Exp} deriving Eq
data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving Eq
data Op2 = And | Or | Xor | Plus deriving Eq


{- Fancy Printer -}
instance Show VName where
  show = init.tail.show.getName
instance (Show VName, Show Exp) => Show Prog where
  show (Lambda (Var x) e) = concat
    ["(lambda (",show x,") ",show e,")"]
  show _ = error "The 1st argument of Prog is not a Var"
instance Show Op1 where
  show Not   = "not"
  show Shl1  = "shl1"
  show Shr1  = "shr1"
  show Shr4  = "shr4"
  show Shr16 = "shr16"
instance Show Op2 where
  show And  = "and"
  show Or   = "or"
  show Xor  = "xor"
  show Plus = "plus"
instance Show VName => Show Exp where
  show None                  = undefined
  show Zero                  = "0"
  show Unit                  = "1"
  show (Val w)               = show w
  show (Var x)       = show x
  show (If p e0 e1)       = intercalate " "
    ["(if0",show p,show e0,(show e1)++")"]
  show (Fold e0 e1 e2) = intercalate " "
    ["(fold",show e0,show e1,"(lambda","("++show (VName "y"),show (VName "z")++")",show e2++"))"]
  show (UnOp op e0)          = intercalate " "
    ["(",show op,(show e0)++")"]
  show (BiOp op e0 e1)       = intercalate " "
    ["("++(show op),show e0,(show e1)++")"]

{- Constructing Sugar -}

-- | 'l' stands for lambda, l = \s -> Var (VName s)
l :: String -> Exp
l str = Var (VName str)


size :: Exp -> Int
size None             = undefined
size Zero             = 1
size Unit             = 1
size (Val _)          = undefined
size (Var _)          = 1
size (If p e0 e1)     = 1+(size p)+(size e0)+(size e1)
size (UnOp _ e0)      = 1+(size e0)
size (BiOp _ e0 e1)   = 1+(size e0)+(size e1)
size (Fold x xs body) =
  2+(size x)+(size xs)+(size body)

hasVar :: Exp -> Bool
hasVar None            = False
hasVar Zero            = False
hasVar Unit            = False
hasVar (Val _)         = False
hasVar (Var _)         = True
hasVar (If p e0 e1)    = (hasVar p) || (hasVar e0) || (hasVar e1)
hasVar (UnOp _ e0)     = (hasVar e0)
hasVar (BiOp _ e0 e1)  = (hasVar e0) || (hasVar e1)
hasVar _               = undefined
