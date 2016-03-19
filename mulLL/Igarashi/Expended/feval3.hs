module Feval3 where
    
    data Term = Lit Int
              | Var Int
              | Add Term Term
              | Abs Term
              | App Term Term
              | Next Term
              | Prev Term
              deriving Show
    data Val = Clos Compt Env
             | Quot Term 
             | Val Int
             deriving Show
    type Env = [Val]
    
    shiftE :: Int -> Env -> Env
    shiftE _ [] = []
    shiftE l (x:xs) = case x of
        --(Clos cp e) -> (Clos (shift 0 l cp) e) : (shiftE l xs)
        (Clos e t) -> (Clos e t) : (shiftE l xs)
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
              | Cont3 Compt Env Cont         -- 0:App
              | Cont4 Compt Env Cont         -- 0:App
              | Cont5 Compt Env Cont         -- 0:Add
              | Cont6 Term Cont             -- 0:Add
              | Cont7 Cont                  -- l:Abs
              | Cont8 Cont                  -- l:Next
              | Cont9 Cont                  -- l:Prev
              | Cont10 Compt Int Env Cont    -- l:App
              | Cont11 Term Cont            -- l:App
              | Cont12 Compt Int Env Cont    -- l:Add
              | Cont13 Term Cont            -- l:Add
    
    
    eval3 :: Term -> Compt
    eval3 (Lit n) = (Number n):[]
    eval3 (Var n) = (Access n):[]
    eval3 (Abs t) = (Lambda (eval3 t)):[]
    eval3 (Next t) = Enter:(eval3 t)
    eval3 (Prev t) = Leave:(eval3 t)
    eval3 (App t1 t2) = (Push (eval3 t2)):(eval3 t1)
    eval3 (Add t1 t2) = (Plus (eval3 t1)):(eval3 t2)
    
    type Compt = [Inst]
    
    data Inst = Number Int
              | Access Int
              | Lambda Compt
              | Push Compt
              | Enter 
              | Leave 
              | Plus Compt
              deriving Show
    
    appC3 :: Compt -> Int -> Env -> Cont -> Val
    appC3 [Number n] 0 e ct = appK3 ct (Val n)                                      -- Cont0
    appC3 [Number n] l e ct = appK3 ct (Quot (Lit n))                               -- Cont0
    appC3 [Access n] 0 e ct = appK3 ct (e!!n)                                       -- Cont0
    appC3 [Access n] l e ct = appK3 ct (Quot (Var n))                               -- Cont0
    appC3 [Lambda cp] 0 e ct = appK3 ct (Clos cp e)                                   -- Cont0
    appC3 [Lambda cp] l e ct = appC3 cp l (shiftE l e) (Cont7 ct)                     -- Cont7
    appC3 (Enter:cp) 0 e ct = appC3 cp 1 e (Cont1 ct)                                -- Cont1
    appC3 (Enter:cp) l e ct = appC3 cp (l+1) e (Cont8 ct)                            -- Cont8
    appC3 (Leave:cp) 1 e ct = appC3 cp 0 e (Cont2 ct)                                -- Cont2
    appC3 (Leave:cp) l e ct = appC3 cp (l-1) e (Cont9 ct)                            -- Cont9
    appC3 ((Push cp2):cp1) 0 e ct = appC3 cp1 0 e (Cont3 cp2 e ct)                  -- Cont3 Cont4
    appC3 ((Push cp2):cp1) l e ct = appC3 cp1 l e (Cont10 cp2 l e ct)               -- Cont10 Cont11
    appC3 ((Plus cp1):cp2) 0 e ct = appC3 cp1 0 e (Cont5 cp2 e ct)                          -- Cont5 Cont6
    appC3 ((Plus cp1):cp2) l e ct = appC3 cp1 l e (Cont12 cp2 l e ct)                       -- Cont12 Con13
    
    
    
    appK3 :: Cont -> Val -> Val
    appK3 (Cont0) val = val
    appK3 (Cont1 ct) (Quot tm) = appK3 ct (Quot tm)
    appK3 (Cont2 ct) (Quot tm) = appK3 ct (Quot tm)
    appK3 (Cont2 ct) (Val n) = appK3 ct (Quot (Lit n))
    appK3 (Cont3 cp2 e ct) (Clos cp1' e') = appC3 cp2 0 e (Cont4 cp1' e' ct)
    appK3 (Cont4 cp1' e' ct) val = appC3 cp1' 0 (val:e') ct
    appK3 (Cont5 cp2 e ct) (Val n) = appC3 cp2 0 e (Cont6 (Lit n) ct)
    appK3 (Cont6 (Lit n) ct) (Val m) = appK3 ct (Val (m + n))
    appK3 (Cont7 ct) (Quot t') = appK3 ct (Quot (Abs t'))
    appK3 (Cont8 ct) (Quot t') = appK3 ct (Quot (Next t'))
    appK3 (Cont9 ct) (Quot t') = appK3 ct (Quot (Prev t'))
    appK3 (Cont10 cp2 l e ct) (Quot t1') = appC3 cp2 l e (Cont11 t1' ct)
    appK3 (Cont11 t1' ct) (Quot t2') = appK3 ct (Quot (App t1' t2'))
    appK3 (Cont12 cp2 l e ct) (Quot t1') = appC3 cp2 l e (Cont13 t1' ct)
    appK3 (Cont13 t1' ct) (Quot t2') = appK3 ct (Quot (Add t1' t2'))
    
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    deq :: Val -> Term
    deq (Quot term) = term
    
    deqC :: Val -> Compt
    deqC (Quot term) = eval3 term
    
    run :: Compt -> Val
    run cp = appC3 cp 0 [] Cont0
    run2 :: Compt -> Val
    run2 = run.deqC.run
    run3 :: Compt -> Val
    run3 = run.deqC.run2
    
    compile :: Term -> Compt
    compile = eval3
    
    exp0 = App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0))
    exp1 = (Next (Abs (Prev (Lit 5))))
    exp2 = (Next (Abs (Prev (Add (Lit 5) (Lit 2)))))
    exp3 = (Next (App (Abs (Add (Var 0) (Lit 5))) (Lit 10)))
    exp4 = (Next (App (Abs (Add (Var 0) (Lit 5))) (Add (Lit 1) (Lit 2))))
    exp5 = (Next (App (Abs (Add (Var 0) (Lit 5))) (Prev (Add (Lit 1) (Lit 2)))))
    --exp6 = (Next (App (Abs (Prev (App (App (Abs (Abs (Next (Add (Lit 5) (Prev (Var 0)))))) (Next (Lit 7))) (Lit 2)))) (Lit 3)))
    exp6 = (Next (App (Abs (Prev (App (App (Abs (Abs (Next (Add (Lit 5) (Prev (Var 1)))))) (Next (Add (Lit 7) (Lit 11)))) (Add (Lit 30) (Lit 2))))) (Lit 3)))
    
    exp7 = (Next (Next (App (Abs (Prev (Prev (Add (Lit 11) (Lit 13))))) (Prev (App (Abs (Var 0)) (Next (Add (Lit 1) (Lit 3))))))))
    exp72 = (Next (Next (App (Abs (Prev (Prev (Add (Lit 11) (Lit 13))))) (Prev (App (Abs (Var 0)) (Next (Next (Add (Lit 1) (Lit 3)))))))))
