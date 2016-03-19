module FEval0 where
    
    data Term = Lit Int
              | Var Int
              | Add Term Term
              | Abs Term
              | App Term Term
              | Next Term
              | Prev Term
              deriving Show
    data Val = Clos Term Env
             | Quot Term
             | Val Int
             deriving Show
    type Env = [Val]
    type Cont = Val -> Val
    
    shiftE :: Int -> Env -> Env
    shiftE _ [] = []
    shiftE l (x:xs) = case x of
        (Clos t e) -> (Clos (shift 0 l t) e) : (shiftE l xs)
        (Quot t) -> (Quot (shift 0 (l-1) t)) : (shiftE l xs)
        (Val n) -> (Val n) : (shiftE l xs)
    
    shift :: Int -> Int -> Term -> Term
    shift j l (Var n) = case (n >= j) && (l == 0) of 
        True -> Var (n+1)
        False -> Var n
    shift j l (Lit n) = Lit n
    shift j l (Add t t') = Add (shift j l t) (shift j l t')
    shift j l (Abs t) = Abs (shift (j+1) l t)
    shift j l (App t0 t1) = App (shift j l t0) (shift j l t1)
    shift j l (Next t) = Next (shift j (l-1) t)
    shift j l (Prev t) = Prev (shift j (l+1) t)
    
    
    
    eval0 :: Term -> Int -> Env -> Cont -> Val
    eval0 (Lit n) 0 e k = k (Val n)
    eval0 (Var n) 0 e k = k (e!!n)
    eval0 (Next t) 0 e k = eval0 t 1 e (\(Quot t') -> k (Quot t'))
    eval0 (Prev t) 1 e k = eval0 t 0 e (\v -> case v of
        (Quot t') -> k (Quot t')
        (Val n) -> k (Quot (Lit n)))
    eval0 (Abs t) 0 e k = k (Clos t e)
    eval0 (App t1 t2) 0 e k = eval0 t1 0 e (\(Clos t1' e') -> 
        eval0 t2 0 e (\val -> 
            eval0 t1' 0 (val:e') k))
    eval0 (Add t1 t2) 0 e k = eval0 t1 0 e (\(Val m) ->
        eval0 t2 0 e (\(Val n) -> 
            k (Val (m + n))))
    eval0 (Lit n) l e k = k (Quot (Lit n))
    eval0 (Var n) l e k = k (Quot (Var n))
    eval0 (Next t) l e k = eval0 t (l+1) e (\(Quot t) -> k (Quot (Next t)))
    eval0 (Prev t) l e k = eval0 t (l-1) e (\(Quot t) -> k (Quot (Prev t)))
    eval0 (Abs t) l e k = eval0 t l (shiftE l e) (\(Quot t) -> k (Quot (Abs t)))
    eval0 (App t1 t2) l e k = 
        eval0 t1 l e (\(Quot  t1') -> 
            eval0 t2 l e (\(Quot t2') -> 
                k (Quot (App t1' t2'))))
    eval0 (Add t1 t2) l e k = 
        eval0 t1 l e (\(Quot  t1') -> 
            eval0 t2 l e (\(Quot t2') -> 
                k (Quot (Add t1' t2'))))
    
    
    deq :: Val -> Term
    deq (Quot t) = t
    
    step :: Term -> Val
    step t = eval0 t 0 [] id
    
    step1 = step
    step2 = step.deq.step
    step3 = step.deq.step2
    step4 = step.deq.step3
    step5 = step.deq.step4
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    
    exp0 = App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0))
    exp1 = (Next (Abs (Prev (Lit 5))))
    exp2 = (Next (Abs (Prev (Add (Lit 5) (Lit 2)))))
    exp3 = (Next (App (Abs (Add (Var 0) (Lit 5))) (Lit 10)))
    exp4 = (Next (App (Abs (Add (Var 0) (Lit 5))) (Add (Lit 1) (Lit 2))))
    exp5 = (Next (App (Abs (Add (Var 0) (Lit 5))) (Prev (Add (Lit 1) (Lit 2)))))
    
    
    {-
    exp6
    => (Next (App (Abs (Add (Var 0) (Next (Add (Lit 5) (Prev (Add (Lit 7) (Lit 3))))))) (Prev (Add (Lit 1) (Lit 2)))))
    tep1 exp6
    => Quot (App (Abs (Add (Var 0) (Next (Add (Lit 5) (Prev (Add (Lit 7) (Lit 3))))))) (Lit 3))
    
    -}
    
    
    
    