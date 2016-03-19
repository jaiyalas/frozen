{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module PigsBloodCake.Core.Syntax where

  import Control.Applicative

  newtype VName = VName {getName :: String}

  instance Show VName where
    show = init.tail.show.getName

  data Prog = Prog {getBinder :: Exp, getBody ::Exp}

  instance (Show VName,Show Exp) => Show Prog where
    show (Prog (Var x) exp) = concat 
      ["(lambda (",show x,") ",show exp,")"]
    show _ = error "The 1st argument of Prog is not a Var"

  data Exp = Zero | Unit | Var VName
    | If   {pred :: Exp, branch_T :: Exp, branch_F :: Exp}
    | Fold Exp Exp Exp Exp Exp
    | UnOp {unop :: Op1, unOpd :: Exp}
    | BiOp {biop :: Op2, biOpdL :: Exp, biOpdR :: Exp}
    deriving Show

  data Op1 = Not | Shl1 | Shr1 | Shl4 | Shl16 deriving Show
  data Op2 = And | Or | Xor | Plus deriving Show

  {- Constructing Sugar -}

  -- | 'l' stands for lambda, l = \s -> Var (VName s)
  l :: String -> Exp
  l str = Var (VName str)
