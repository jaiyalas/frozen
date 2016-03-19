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
              | Cont10 Compt Env Cont    -- l:App
              | Cont11 Term Cont            -- l:App
              | Cont12 Compt Env Cont    -- l:Add
              | Cont13 Term Cont            -- l:Add
    
    type Compt = [Inst]
    
    data Inst = Number Int
            | Access Int
            | Lambda Compt
            | Push Compt
            | Enter 
            | Leave 
            | Plus Compt
            | QLit Int
            | QVar Int
            | PushQAbs Compt Int -- my edition ; | PushQAbs Int -- Oleg's
            | PushQApp Compt 
            | PushQNext 
            | PushQPrev 
            | PushQAdd Compt
            deriving Show
    
    eval3 :: Term -> Int -> Compt
    -- level = 0
    eval3 (Lit n) 0 = (Number n):[]
    eval3 (Var n) 0 = (Access n):[]
    eval3 (Abs t) 0 = (Lambda (eval3 t 0)):[]
    eval3 (Next t) 0 = Enter:(eval3 t 1)
    eval3 (App t1 t2) 0 = (Push (eval3 t2 0)):(eval3 t1 0)
    eval3 (Add t1 t2) 0 = (Plus (eval3 t1 0)):(eval3 t2 0)
    -- level > 0
    eval3 (Prev t) 1 = Leave:(eval3 t 0)
    eval3 (Lit n) l = (QLit n):[]
    eval3 (Var n) l = (QVar n):[]
    eval3 (Abs t) l = (PushQAbs (eval3 t l) l):[]
    eval3 (Next t) l = PushQNext:(eval3 t (l+1))
    eval3 (Prev t) l = PushQPrev:(eval3 t (l-1))
    eval3 (App t1 t2) l = (PushQApp (eval3 t2 l)):(eval3 t1 l)
    eval3 (Add t1 t2) l = (PushQAdd (eval3 t1 l)):(eval3 t2 l)
    
    appC3 :: Compt -> Env -> Cont -> Val
    -- appC -- interpretation mode
    appC3 [Number n] e ct = appK3 ct (Val n)  
    appC3 [Access n] e ct = appK3 ct (e!!n)       
    appC3 [Lambda cp] e ct = appK3 ct (Clos cp e)   
    appC3 (Enter:cp) e ct = appC3 cp e (Cont1 ct)           -- (* mode switch*)  -
    appC3 ((Push cp2):cp1) e ct = appC3 cp1 e (Cont3 cp2 e ct)         
    appC3 ((Plus cp1):cp2) e ct = appC3 cp1 e (Cont5 cp2 e ct)   
    -- appCQ -- compilation mode
    appC3 (Leave:cp) e ct = appC3 cp e (Cont2 ct)               -- (* mode switch*)
    appC3 [QVar n] e ct = appK3 ct (Quot (Var n)) 
    appC3 [QLit n] e ct = appK3 ct (Quot (Lit n))  
    appC3 [PushQAbs cp l] e ct = appC3 cp (shiftE l e) (Cont7 ct)
    appC3 (PushQNext:cp) e ct = appC3 cp e (Cont8 ct)   
    appC3 (PushQPrev:cp) e ct = appC3 cp e (Cont9 ct)  
    appC3 ((PushQApp cp2):cp1) e ct = appC3 cp1 e (Cont10 cp2 e ct) 
    appC3 ((PushQAdd cp1):cp2) e ct = appC3 cp1 e (Cont12 cp2 e ct)  
    
    appK3 :: Cont -> Val -> Val
    appK3 (Cont0) val = val
    -- appK -- interpretation mode
    appK3 (Cont1 ct) (Quot tm) = appK3 ct (Quot tm) -- (* mode switch *)
    appK3 (Cont3 cp2 e ct) (Clos cp1' e') = appC3 cp2 e (Cont4 cp1' e' ct)
    appK3 (Cont4 cp1' e' ct) val = appC3 cp1' (val:e') ct
    appK3 (Cont5 cp2 e ct) (Val n) = appC3 cp2 e (Cont6 (Lit n) ct)
    appK3 (Cont6 (Lit n) ct) (Val m) = appK3 ct (Val (m + n))
    -- appKQ -- compilation mode
    appK3 (Cont2 ct) (Quot tm) = appK3 ct (Quot tm) -- (* mode switch *)
    appK3 (Cont2 ct) (Val n) = appK3 ct (Quot (Lit n)) --(* mode switch *)
    appK3 (Cont7 ct) (Quot t') = appK3 ct (Quot (Abs t'))
    appK3 (Cont8 ct) (Quot t') = appK3 ct (Quot (Next t'))
    appK3 (Cont9 ct) (Quot t') = appK3 ct (Quot (Prev t'))
    appK3 (Cont10 cp2 e ct) (Quot t1') = appC3 cp2 e (Cont11 t1' ct)
    appK3 (Cont11 t1' ct) (Quot t2') = appK3 ct (Quot (App t1' t2'))
    appK3 (Cont12 cp2 e ct) (Quot t1') = appC3 cp2 e (Cont13 t1' ct)
    appK3 (Cont13 t1' ct) (Quot t2') = appK3 ct (Quot (Add t1' t2'))
    
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    deq :: Val -> Term
    deq (Quot term) = term
    
    deqC :: Val -> Compt
    deqC (Quot term) = eval3 term 0
    
    run :: Compt -> Val
    run cp = appC3 cp [] Cont0
    run2 :: Compt -> Val
    run2 = run.deqC.run
    run3 :: Compt -> Val
    run3 = run.deqC.run2
    
    compile :: Term -> Compt
    compile = (flip eval3) 0
    
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
    exp73 = (Next (Next (App (Abs (Prev (Prev (Add (Next (Next (Var 0))) (Lit 13))))) (Prev (App (Abs (Var 0)) (Next (Next (Add (Lit 1) (Lit 3)))))))))
    
    
    
    t3 = (Next (Abs (Next (Abs (App (Prev (Prev (Next (Var 0)))) (Prev (Next (Var 0))))))))
