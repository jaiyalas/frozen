module FEval1 where
    
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
    
    
    eval1 :: Term -> Int -> Env -> Cont -> Val
    eval1 (Lit n) 0 e k = appK1 k (Val n)                       -- Cont0
    eval1 (Var n) 0 e k = appK1 k (e!!n)                        -- Cont0
    eval1 (Abs t) 0 e k = appK1 k (Clos t e)                    -- Cont0
    eval1 (Next t) 0 e k = eval1 t 1 e (Cont1 k)                -- Cont1
    eval1 (Prev t) 1 e k = eval1 t 0 e (Cont2 k)                -- Cont2
    eval1 (App t1 t2) 0 e k = eval1 t1 0 e (Cont3 t2 e k)       -- Cont3 Cont4
    eval1 (Add t1 t2) 0 e k = eval1 t1 0 e (Cont5 t2 e k)       -- Cont5 Cont6
    eval1 (Lit n) l e k = appK1 k (Quot (Lit n))                -- Cont0
    eval1 (Var n) l e k = appK1 k (Quot (Var n))                -- Cont0
    eval1 (Abs t) l e k = eval1 t l (shiftE l e) (Cont7 k)      -- Cont7
    eval1 (Next t) l e k = eval1 t (l+1) e (Cont8 k)            -- Cont8
    eval1 (Prev t) l e k = eval1 t (l-1) e (Cont9 k)            -- Cont9
    eval1 (App t1 t2) l e k = eval1 t1 l e (Cont10 t2 l e k)    -- Cont10 Cont11
    eval1 (Add t1 t2) l e k = eval1 t1 l e (Cont12 t2 l e k)    -- Cont12 Con13
    
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
    
    appK1 :: Cont -> Val -> Val
    appK1 (Cont0) val = val
    appK1 (Cont1 ct) (Quot tm) = appK1 ct (Quot tm)
    appK1 (Cont2 ct) (Quot tm) = appK1 ct (Quot tm)
    appK1 (Cont2 ct) (Val n) = appK1 ct (Quot (Lit n))
    appK1 (Cont3 t2 e ct) (Clos t1' e') = eval1 t2 0 e (Cont4 t1' e' ct)
    appK1 (Cont4 t1' e' ct) val = eval1 t1' 0 (val:e') ct
    appK1 (Cont5 t2 e ct) (Val n) = eval1 t2 0 e (Cont6 (Lit n) ct)
    appK1 (Cont6 (Lit n) ct) (Val m) = appK1 ct (Val (m + n))
    appK1 (Cont7 ct) (Quot t') = appK1 ct (Quot (Abs t'))
    appK1 (Cont8 ct) (Quot t') = appK1 ct (Quot (Next t'))
    appK1 (Cont9 ct) (Quot t') = appK1 ct (Quot (Prev t'))
    appK1 (Cont10 t2 l e ct) (Quot t1') = eval1 t2 l e (Cont11 t1' ct)
    appK1 (Cont11 t1' ct) (Quot t2') = appK1 ct (Quot (App t1' t2'))
    appK1 (Cont12 t2 l e ct) (Quot t1') = eval1 t2 l e (Cont13 t1' ct)
    appK1 (Cont13 t1' ct) (Quot t2') = appK1 ct (Quot (Add t1' t2'))
    
    deq :: Val -> Term
    deq (Quot t) = t
    
    step :: Term -> Val
    step t = eval1 t 0 [] Cont0
    
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
    
    
    