module Eval1 where
    
    -- defunctionalizing continuations
    
    data Term = Var Int
              | Abs Term
              | App Term Term
              | Next Term
              | Prev Term
              deriving Show
    data Val = Clos Env Term
             | Quot Term
             deriving Show
    type Env = [Val]
    
    shiftE :: Int -> Env -> Env
    shiftE _ [] = []
    shiftE l (x:xs) = case x of
        (Clos e t) -> (Clos e (shift 0 l t)) : (shiftE l xs)
        (Quot t) -> (Quot (shift 0 (l-1) t)) : (shiftE l xs)
    
    shift :: Int -> Int -> Term -> Term
    shift j l (Var n) = case (n >= j) && (l == 0) of 
        True -> Var (n+1)
        False -> Var n
    shift j l (Abs t) = Abs (shift (j+1) l t)
    shift j l (App t0 t1) = App (shift j l t0) (shift j l t1)
    shift j l (Next t) = Next (shift j (l-1) t)
    shift j l (Prev t) = Prev (shift j (l+1) t)
    
    data Cont = Cont0 
              | Cont1 Term Env Cont
              | Cont2 Term Env Cont
              | Cont3 Cont
              | Cont4 Cont
              | Cont5 Cont
              | Cont6 Term Int Env Cont
              | Cont7 Term Cont
              | Cont8 Cont
              | Cont9 Cont

    eval1 :: Term -> Int -> Env -> Cont -> Val

    eval1 (Var n) 0 e k = appK1 k (e !! n)
    eval1 (Abs t) 0 e k = appK1 k (Clos e t)
    eval1 (App t0 t1) 0 e k = eval1 t0 0 e (Cont1 t1 e k)
    eval1 (Next t) 0 e k = eval1 t 1 e (Cont3 k)
    eval1 (Prev t) 1 e k = eval1 t 0 e (Cont4 k)
    eval1 (Var n) l e k = appK1 k (Quot (Var n))
    eval1 (Abs t) l e k = eval1 t l (shiftE l e) (Cont5 k)
    eval1 (App t0 t1) l e k = eval1 t0 l e (Cont6 t1 l e k)
    eval1 (Next t) l e k = eval1 t (l+1) e (Cont8 k)
    eval1 (Prev t) l e k = eval1 t (l-1) e (Cont9 k)


    appK1 :: Cont -> Val -> Val
    appK1 Cont0 v = v
    appK1 (Cont1 t e k) (Clos e' t') = eval1 t 0 e (Cont2 t' e' k)
    appK1 (Cont2 t e k) v = eval1 t 0 (v:e) k
    appK1 (Cont3 k) (Quot t) = appK1 k (Quot t)
    appK1 (Cont4 k) (Quot t) = appK1 k (Quot t)
    appK1 (Cont5 k) (Quot t) = appK1 k (Quot (Abs t))
    appK1 (Cont6 t1 l e k) (Quot t2) = eval1 t1 l e (Cont7 t2 k)
    appK1 (Cont7 t2 k) (Quot t3) = appK1 k (Quot (App t2 t3))
    appK1 (Cont8 k) (Quot t) = appK1 k (Quot (Next t))
    appK1 (Cont9 k) (Quot t) = appK1 k (Quot t)
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    
    
    
    
    
    
    
