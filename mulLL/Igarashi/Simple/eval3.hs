module Eval3 where
    
    
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
        (Quot t) -> (Quot (shift 0 (l-1) t)) : (shiftE l xs)
    
    shift :: Int -> Int -> Term -> Term
    shift j l (Var n) = case (n >= j) && (l == 0) of 
        True -> Var (n+1)
        False -> Var n
    shift j l (Abs t) = Abs (shift (j+1) l t)
    shift j l (App t0 t1) = App (shift j l t0) (shift j l t1)
    shift j l (Next t) = Next (shift j (l-1) t)
    shift j l (Prev t) = Prev (shift j (l+1) t)
    
    type Compt = [Inst]
    
    data Inst = Inst0 Int
              | Inst1 Compt
              | Inst2 Compt
              | Inst3 
              | Inst4
              deriving Show
    
    data Val = Clos Env Compt
             | Quot Term
    
    instance Show Val where
        show (Clos e compt) = "Clos "++(show e)++" COMPT"
        show (Quot t) = "Quot "++(show t)
    
    data Cont = Cont0 
              | Cont1 Compt Env Cont
              | Cont2 Compt Env Cont
              | Cont3 Cont
              | Cont4 Cont
              | Cont5 Cont
              | Cont6 Compt Int Env Cont
              | Cont7 Term Cont
              | Cont8 Cont
              | Cont9 Cont
    
    
    eval3 :: Term -> Compt
    eval3 (Var n) = [Inst0 n]
    eval3 (Abs t) = [Inst1 (eval3 t)]
    eval3 (App t t') = Inst2 (eval3 t) : (eval3 t')
    eval3 (Next t) = Inst3 : (eval3 t)
    eval3 (Prev t) = Inst4 : (eval3 t)
    
    appC3 :: Compt -> Int -> Env -> Cont -> Val
    appC3 ([Inst0 n]) 0 e k = appK3 k (e !! n)
    appC3 ([Inst0 n]) l e k = appK3 k (Quot (Var n))
    appC3 ([Inst1 cp]) 0 e k = appK3 k (Clos e cp)
    appC3 ([Inst1 cp]) l e k = appC3 cp l (shiftE l e) (Cont5 k)
    appC3 (Inst2 cp0 : cp1) 0 e k = appC3 cp0 0 e (Cont1 cp1 e k)
    appC3 (Inst2 cp0 : cp1) l e k = appC3 cp0 l e (Cont6 cp1 l e k)
    appC3 (Inst3 : cp) 0 e k = appC3 cp 1 e (Cont3 k)
    appC3 (Inst3 : cp) l e k = appC3 cp (l+1) e (Cont8 k)
    appC3 (Inst4 : cp) 0 e k = appC3 cp 0 e (Cont4 k)
    appC3 (Inst4 : cp) l e k = appC3 cp (l-1) e (Cont9 k)
    
    appK3 :: Cont -> Val -> Val
    appK3 Cont0 v = v
    appK3 (Cont1 cp e k) (Clos e' cp') = appC3 cp 0 e (Cont2 cp' e' k)
    appK3 (Cont2 cp e k) v = appC3 cp 0 (v:e) k
    appK3 (Cont3 k) (Quot t) = appK3 k (Quot t)
    appK3 (Cont4 k) (Quot t) = appK3 k (Quot t)
    appK3 (Cont5 k) (Quot t) = appK3 k (Quot (Abs t))
    appK3 (Cont6 cp l e k) (Quot t2) = appC3 cp l e (Cont7 t2 k) 
    appK3 (Cont7 t2 k) (Quot t3) = appK3 k (Quot (App t2 t3)) 
    appK3 (Cont8 k) (Quot t) = appK3 k (Quot (Next t))
    appK3 (Cont9 k) (Quot t) = appK3 k (Quot t)
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
