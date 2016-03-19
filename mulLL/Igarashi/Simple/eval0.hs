module Eval0 where
    
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
    type Cont = Val -> Val
    
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
    
    
    eval0 :: Term -> Int -> Env -> Cont -> Val
    eval0 (Var n) 0 e k = k (e !! n)
    eval0 (Abs t0) 0 e k = k (Clos e t0)
    eval0 (App t0 t1) 0 e k = 
        eval0 t0 0 e (\(Clos e' t') -> 
            eval0 t1 0 e (\v -> 
                eval0 t' 0 (v : e') k))
    eval0 (Next t) 0 e k = eval0 t 1 e (\(Quot t') -> k (Quot t'))
    eval0 (Prev t) 1 e k = eval0 t 0 e (\(Quot t') -> k (Quot t'))
    eval0 (Var n) l e k = k (Quot (Var n))
    eval0 (Abs t) l e k = eval0 t l (shiftE l e) (\(Quot t) -> k (Quot (Abs t)))
    eval0 (App t0 t1) l e k = 
        eval0 t0 l e (\(Quot  t2) -> 
            eval0 t1 l e (\(Quot t3) -> 
                k (Quot (App t2 t3))))
    eval0 (Next t) l e k = eval0 t (l+1) e (\(Quot t) -> k (Quot (Next t)))
    eval0 (Prev t) l e k = eval0 t (l-1) e (\(Quot t) -> k (Quot t))
    
    
    
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    fig2' = Next (Abs (App (Prev (Abs (Next (Abs (Prev (Var 0)))))) (Next (Var 0))))
    
    