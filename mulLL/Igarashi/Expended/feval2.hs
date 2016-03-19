module Feval2 where
    
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
    
    data Cont = Cont0                       -- 0:Lit 0:Var 0:Abs l:Lit l:Var 
              | Cont1 Cont                  -- 0:Next 
              | Cont2 Cont                  -- 0:Prev 
              | Cont3 Term Env Cont         -- 0:App
              | Cont4 Term Env Cont         -- 0:App
              | Cont5 Term Env Cont         -- 0:Add
              | Cont6 Term Cont             -- 0:Add
              | Cont7 Cont                  -- l:Abs
              | Cont8 Cont                  -- l:Next
              | Cont9 Cont                  -- l:Prev
              | Cont10 Term Int Env Cont    -- l:App
              | Cont11 Term Cont            -- l:App
              | Cont12 Term Int Env Cont    -- l:Add
              | Cont13 Term Cont            -- l:Add
    
    type Compt = Int -> Env -> Cont -> Val
    
    eval2 :: Term -> Compt
    eval2 (Lit n) = \l e k -> if (l==0) 
        then appK2 k (Val n)                                      -- Cont0
        else appK2 k (Quot (Lit n))                               -- Cont0
    eval2 (Var n) = \l e k -> if (l==0) 
        then appK2 k (e!!n)                                       -- Cont0
        else appK2 k (Quot (Var n))                               -- Cont0
    eval2 (Abs t) = \l e k -> if (l==0) 
        then appK2 k (Clos t e)                                   -- Cont0
        else eval2 t l (shiftE l e) (Cont7 k)                     -- Cont7
    eval2 (Next t) = \l e k -> if (l==0) 
        then eval2 t 1 e (Cont1 k)                                -- Cont1
        else eval2 t (l+1) e (Cont8 k)                            -- Cont8
    eval2 (Prev t) = \l e k -> if (l==1) 
        then eval2 t 0 e (Cont2 k)                                -- Cont2
        else eval2 t (l-1) e (Cont9 k)                            -- Cont9
    eval2 (App t1 t2) = \l e k -> if (l==0) 
        then eval2 t1 0 e (Cont3 t2 e k)                          -- Cont3 Cont4
        else eval2 t1 l e (Cont10 t2 l e k)                       -- Cont10 Cont11
    eval2 (Add t1 t2) = \l e k -> if (l==0) 
        then eval2 t1 0 e (Cont5 t2 e k)                          -- Cont5 Cont6
        else eval2 t1 l e (Cont12 t2 l e k)                       -- Cont12 Con13
    
    appK2 :: Cont -> Val -> Val
    appK2 (Cont0) val = val
    appK2 (Cont1 ct) (Quot tm) = appK2 ct (Quot tm)
    appK2 (Cont2 ct) (Quot tm) = appK2 ct (Quot tm)
    appK2 (Cont2 ct) (Val n) = appK2 ct (Quot (Lit n))
    appK2 (Cont3 t2 e ct) (Clos t1' e') = eval2 t2 0 e (Cont4 t1' e' ct)
    appK2 (Cont4 t1' e' ct) val = eval2 t1' 0 (val:e') ct
    appK2 (Cont5 t2 e ct) (Val n) = eval2 t2 0 e (Cont6 (Lit n) ct)
    appK2 (Cont6 (Lit n) ct) (Val m) = appK2 ct (Val (m + n))
    appK2 (Cont7 ct) (Quot t') = appK2 ct (Quot (Abs t'))
    appK2 (Cont8 ct) (Quot t') = appK2 ct (Quot (Next t'))
    appK2 (Cont9 ct) (Quot t') = appK2 ct (Quot (Prev t'))
    appK2 (Cont10 t2 l e ct) (Quot t1') = eval2 t2 l e (Cont11 t1' ct)
    appK2 (Cont11 t1' ct) (Quot t2') = appK2 ct (Quot (App t1' t2'))
    appK2 (Cont12 t2 l e ct) (Quot t1') = eval2 t2 l e (Cont13 t1' ct)
    appK2 (Cont13 t1' ct) (Quot t2') = appK2 ct (Quot (Add t1' t2'))
    
    
    deq :: Val -> Term
    deq (Quot t) = t
    
    step :: Term -> Val
    step t = eval2 t 0 [] Cont0
    
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
    
    
