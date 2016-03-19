module Eval4 where
    
    
    data Term = Var Int
              | Abs Term
              | App Term Term
              | Next Term
              | Prev Term
              deriving Show
              
    type Env = [Val]
    
    shiftE :: Int -> Env -> Env
    shiftE _ [] = []
    shiftE l (x:xs) = case x of
        --(Clos e t) -> (Clos e (shift 0 l t)) : (shiftE l xs)
        (Clos e t) -> (Clos e t) : (shiftE l xs)
        (Quot t) -> (Quot (shift_compt 0 (l-1) t)) : (shiftE l xs)
    
    shift_compt :: Int -> Int -> Compt -> Compt
    shift_compt _ _ [] = []
    shift_compt j l (i:is) = (shift_inst j l i):(shift_compt j l is)

    shift_inst :: Int -> Int -> Inst -> Inst
    shift_inst j l (Access n) = case (n >= j) && (l == 0) of 
      True -> Access (n+1)
      False -> Access n
    shift_inst j l (Close cp) = Close (shift_compt (j+1) l cp)
    shift_inst j l (Push cp) = (Push (shift_compt j l cp))
    shift_inst j l (Enter) = Enter
    shift_inst j l (Leave) = Leave
    
    type Compt = [Inst]
    
    data Inst = Access Int
              | Close Compt
              | Push Compt
              | Enter 
              | Leave
              deriving Show
    
    data Val = Clos Env Compt
             | Quot Compt
             deriving Show
    
    
    data Cont = Helf 
              | EvArg Compt Env Cont
              | EvBody Compt Env Cont
              | Quote Cont
              | Unquote Cont
              | QAbs Cont
              | QApp' Compt Int Env Cont
              | QApp Compt Cont
              | QNext Cont
              | QPrev Cont
              deriving Show
    
    
    eval4 :: Term -> Compt
    eval4 (Var n) = [Access n]
    eval4 (Abs t) = [Close (eval4 t)]
    eval4 (App t t') = Push (eval4 t) : (eval4 t')
    eval4 (Next t) = Enter : (eval4 t)
    eval4 (Prev t) = Leave : (eval4 t)
    
    appC4 :: Compt -> Int -> Env -> Cont -> Val
    appC4 ([Access n]) 0 e k = appK4 k (e !! n)
    appC4 ([Access n]) l e k = appK4 k (Quot [Access n])  --
    appC4 ([Close cp]) 0 e k = appK4 k (Clos e cp)
    appC4 ([Close cp]) l e k = appC4 cp l (shiftE l e) (QAbs k)
    appC4 (Push cp0 : cp1) 0 e k = appC4 cp0 0 e (EvArg cp1 e k)
    appC4 (Push cp0 : cp1) l e k = appC4 cp0 l e (QApp' cp1 l e k)
    appC4 (Enter : cp) 0 e k = appC4 cp 1 e (Quote k)
    appC4 (Enter : cp) l e k = appC4 cp (l+1) e (QNext k)
    appC4 (Leave : cp) 0 e k = appC4 cp 0 e (Unquote k)
    appC4 (Leave : cp) l e k = appC4 cp (l-1) e (QPrev k)
    
    appK4 :: Cont -> Val -> Val
    appK4 Helf v = v
    appK4 (EvArg cp e k) (Clos e' cp') = appC4 cp 0 e (EvBody cp' e' k)
    appK4 (EvBody cp e k) v = appC4 cp 0 (v:e) k
    appK4 (Quote k) (Quot cp) = appK4 k (Quot cp)
    appK4 (Unquote k) (Quot cp) = appK4 k (Quot cp)
    appK4 (QAbs k) (Quot cp) = appK4 k (Quot [Close cp])
    appK4 (QApp' cp l e k) (Quot cp2) = appC4 cp l e (QApp cp2 k) 
    appK4 (QApp cp2 k) (Quot cp3) = appK4 k (Quot ((Push cp2):cp3)) 
    appK4 (QNext k) (Quot cp) = appK4 k (Quot (Enter:cp))
    appK4 (QPrev k) (Quot cp) = appK4 k (Quot cp)
    
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    com = eval4 fig2
    
    takeCompt :: Val -> Compt
    takeCompt (Clos env compt) = compt
    takeCompt (Quot compt) = compt
    
    test = appC4 com 0 [] Helf
    test2 = appC4 (takeCompt test) 0 [] Helf
    
    try = appC4 (eval4 (Next (App (Abs (Var 0)) (Var 0)))) 0 [] Helf
    
