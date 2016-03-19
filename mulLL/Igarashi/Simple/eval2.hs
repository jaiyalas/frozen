module Eval2 where
    
    -- currying transformation
    
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
    
    type Compt = Int -> Env -> Cont -> Val
    
    
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
    
    
    eval2 :: Term -> Compt
    eval2 (Var n) = (\l e k -> if (l==0) 
        then appK2 k (e !! n)
        else appK2 k (Quot (Var n)))
    eval2 (Abs t) = (\l e k -> let cp = eval2 t in
        if (l==0) 
            then appK2 k (Clos e cp)
            else cp l (shiftE l e) (Cont5 k)) -- 
    eval2 (App t0 t1) = (\l e k -> let (cp0,cp1) = (eval2 t0,eval2 t1) in
        if (l==0)
            then cp0 0 e (Cont1 cp1 e k)
            else cp0 l e (Cont6 cp1 l e k))
    eval2 (Next t) = (\l e k -> let cp = eval2 t in
        if (l==0)
            then cp 1 e (Cont3 k)
            else cp (l+1) e (Cont8 k))
    eval2 (Prev t) = (\l e k -> let cp = eval2 t in
        if (l==1)
            then cp 0 e (Cont4 k)
            else cp (l-1) e (Cont9 k))
    
    appK2 :: Cont -> Val -> Val
    appK2 Cont0 v = v
    appK2 (Cont1 cp e k) (Clos e' t') = cp 0 e (Cont2 t' e' k)
    appK2 (Cont2 cp e k) v = cp 0 (v:e) k
    appK2 (Cont3 k) (Quot t) = appK2 k (Quot t)
    appK2 (Cont4 k) (Quot t) = appK2 k (Quot t)
    appK2 (Cont5 k) (Quot t) = appK2 k (Quot (Abs t))
    appK2 (Cont6 cp l e k) (Quot t2) = cp l e (Cont7 t2 k)
    appK2 (Cont7 t2 k) (Quot t3) = appK2 k (Quot (App t2 t3))
    appK2 (Cont8 k) (Quot t) = appK2 k (Quot (Next t))
    appK2 (Cont9 k) (Quot t) = appK2 k (Quot t)
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
