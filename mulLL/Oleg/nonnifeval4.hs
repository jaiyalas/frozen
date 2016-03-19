module Feval42 where
    
    data Term = Lit Int
              | Var Int
              | Add Term Term
              | Abs Term
              | App Term Term
              | Next Term
              | Prev Term
              deriving Show
    data Val = Clos Compt Env
             | Quot Compt 
             | Val Int
             deriving Show
    type Env = [Val]
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
              | PushQAbs Compt Int
              | PushQApp Compt 
              | PushQNext 
              | PushQPrev 
              | PushQAdd Compt
              deriving Show
    
    data Cont = Cont0                       -- 0:Lit 0:Var 0:Abs l:Lit l:Var 
              | Cont1 Cont                  -- 0:Next 
              | Cont2 Cont                  -- 0:Prev 
              | Cont3 Compt Env Cont         -- 0:App
              | Cont4 Compt Env Cont         -- 0:App
              | Cont5 Compt Env Cont         -- 0:Add
              | Cont6 Compt Cont             -- 0:Add
              | Cont7 Cont Int                  -- l:Abs
              | Cont8 Cont                  -- l:Next
              | Cont9 Cont                  -- l:Prev
              | Cont10 Compt Env Cont    -- l:App
              | Cont11 Compt Cont            -- l:App
              | Cont12 Compt Env Cont    -- l:Add
              | Cont13 Compt Cont            -- l:Add
    
    shiftE :: Int -> Env -> Env
    shiftE _ [] = []
    shiftE l (x:xs) = case x of
        (Clos cp e) -> (Clos (shift_compt 0 l cp) e) : (shiftE l xs)
        (Quot cp) -> (Quot (shift_compt 0 (l-1) cp)) : (shiftE l xs)
        (Val n) -> (Val n) : (shiftE l xs)
    
    shift_compt :: Int -> Int -> Compt -> Compt
    shift_compt j l = map (shift_inst j l)
    
    shift_inst :: Int -> Int -> Inst -> Inst
    shift_inst j l (Number n) = Number n
    shift_inst j l (Access n) = case (n >= j) && (l == 0) of 
      True -> Access (n+1)
      False -> Access n
    shift_inst j l (Lambda cp) = Lambda (shift_compt (j+1) l cp)
    shift_inst j l (Push cp) = Push (shift_compt j l cp)
    shift_inst j l (Enter) = Enter
    shift_inst j l (Leave) = Leave
    shift_inst j l (Plus cp) = Plus (shift_compt j l cp)
    
    {- shift :: Int -> Int -> Term -> Term
    shift j l (Var n) = case (n >= j) && (l == 0) of 
        True -> Var (n+1)
        False -> Var n
    shift j l (Lit n) = Lit n
    shift j l (Add t t') = Add (shift j l t) (shift j l t')
    shift j l (Abs t) = Abs (shift (j+1) l t)
    shift j l (App t0 t1) = App (shift j l t0) (shift j l t1)
    shift j l (Next t) = Next (shift j (l-1) t)
    shift j l (Prev t) = Prev (shift j (l+1) t) -}
    
    
    eval4 :: Term -> Int -> Compt
    -- Level = 0
    eval4 (Lit n) 0 = (Number n):[]
    eval4 (Var n) 0 = (Access n):[]
    eval4 (Abs t) 0 = (Lambda (eval4 t 0)):[]
    eval4 (Next t) 0 = Enter:(eval4 t 1)
    eval4 (Prev t) 1 = Leave:(eval4 t 0)
    eval4 (App t1 t2) 0 = (Push (eval4 t2 0)):(eval4 t1 0)
    eval4 (Add t1 t2) 0 = (Plus (eval4 t1 0)):(eval4 t2 0)
    -- Level > 0
    eval4 (Lit n) l = (QLit n):[]
    eval4 (Var n) l = (QVar n):[]
    eval4 (Abs t) l = (PushQAbs (eval4 t l) l):[]
    eval4 (Next t) l = PushQNext:(eval4 t (l+1))
    eval4 (Prev t) l = PushQPrev:(eval4 t (l-1))
    eval4 (App t1 t2) l = (PushQApp (eval4 t2 l)):(eval4 t1 l)
    eval4 (Add t1 t2) l = (PushQAdd (eval4 t1 l)):(eval4 t2 l)
    
    
    appC4 :: Compt -> Env -> Cont -> Val
    -- Interpretation 
    appC4 [Number n] e ct = appK4 ct (Val n) 
    appC4 [Access n] e ct = appK4 ct (e!!n)  
    appC4 [Lambda cp] e ct = appK4 ct (Clos cp e) 
    appC4 (Enter:cp) e ct = appC4 cp e (Cont1 ct)              -- switch mode
    appC4 ((Push cp2):cp1) e ct = appC4 cp1 e (Cont3 cp2 e ct)
    appC4 ((Plus cp1):cp2) e ct = appC4 cp1 e (Cont5 cp2 e ct)  
    -- Compilation 
    appC4 (Leave:cp) e ct = appC4 cp e (Cont2 ct)           -- switch mode
    appC4 [QLit n] e ct = appK4 ct (Quot [QLit n]) 
    appC4 [QVar n] e ct = appK4 ct (Quot [QVar n])  
    appC4 [PushQAbs cp l] e ct = appC4 cp (shiftE l e) (Cont7 ct l) 
    appC4 (PushQNext:cp) e ct = appC4 cp e (Cont8 ct) 
    appC4 (PushQPrev:cp) e ct = appC4 cp e (Cont9 ct) 
    appC4 ((PushQApp cp2):cp1) e ct = appC4 cp1 e (Cont10 cp2 e ct) 
    appC4 ((PushQAdd cp1):cp2) e ct = appC4 cp1 e (Cont12 cp2 e ct)  
    
    
    appK4 :: Cont -> Val -> Val
    appK4 (Cont0) val = val
    -- Interpretation 
    appK4 (Cont1 ct) (Quot cp) = appK4 ct (Quot cp)           -- switch mode
    appK4 (Cont3 cp2 e ct) (Clos cp1' e') = appC4 cp2 e (Cont4 cp1' e' ct)
    appK4 (Cont4 cp1' e' ct) val = appC4 cp1' (val:e') ct
    appK4 (Cont5 cp2 e ct) (Val n) = appC4 cp2 e (Cont6 [Number n] ct)
    appK4 (Cont6 [Number n] ct) (Val m) = appK4 ct (Val (m + n)) 
    -- Compilation 
    appK4 (Cont2 ct) (Quot cp) = appK4 ct (Quot cp)          -- switch mode
    appK4 (Cont2 ct) (Val n) = appK4 ct (Quot [QLit n])    -- switch mode
    appK4 (Cont7 ct l) (Quot cp') = appK4 ct (Quot [PushQAbs cp' l])
    appK4 (Cont8 ct) (Quot cp') = appK4 ct (Quot (PushQNext:cp'))
    appK4 (Cont9 ct) (Quot cp') = appK4 ct (Quot (PushQPrev:cp'))
    appK4 (Cont10 cp2 e ct) (Quot cp1') = appC4 cp2 e (Cont11 cp1' ct)
    appK4 (Cont11 cp1' ct) (Quot cp2') = appK4 ct (Quot ((PushQApp cp2'):cp1'))
    appK4 (Cont12 cp2 e ct) (Quot cp1') = appC4 cp2 e (Cont13 cp1' ct)
    appK4 (Cont13 cp1' ct) (Quot cp2') = appK4 ct (Quot ((PushQAdd cp1'):cp2'))
    
    
    demote :: Int -> Compt -> Compt
    demote _ [] = []
    
    demote l ((Number n):cs) = (Number n):(demote l cs)
    demote l ((Access n):cs) = (Access n):(demote l cs)
    --demote l (Lambda cp):cs = (Lambda (demotw l cp)):(demote l cs)
    --demote l (Push cp):cs = (Push (demotw l cp)):(demote l cs)
    --demote l (Plus cp):cs = (Plus (demotw l cp)):(demote l cs)
    
    demote 1 ((QLit n):cs) = (Number n):(demote 1 cs)
    demote 1 ((QVar n):cs) = (Access n):(demote 1 cs)
    demote 1 ((PushQAbs cp 1):cs) = (Lambda (demote 1 cp)):(demote 1 cs)
    demote 1 ((PushQApp cp):cs) = (Push (demote 1 cp)):(demote 1 cs)
    demote 1 ((PushQAdd cp):cs) = (Plus (demote 1 cp)):(demote 1 cs)
    
    demote 1 ((PushQNext):cs) = (Enter):(demote 2 cs)
    demote 2 ((PushQPrev):cs) = (Leave):(demote 1 cs)
    
    demote l ((QLit n):cs) = (QLit n):(demote l cs)
    demote l ((QVar n):cs) = (QVar n):(demote l cs)
    demote l ((PushQAbs cp n):cs) = (PushQAbs (demote l cp) (n-1)):(demote l cs)
    demote l ((PushQApp cp):cs) = (PushQApp (demote l cp)):(demote l cs)
    demote l ((PushQAdd cp):cs) = (PushQAdd (demote l cp)):(demote l cs)
    
    demote l ((PushQNext):cs) = (PushQNext):(demote (l+1) cs)
    demote l ((PushQPrev):cs) = (PushQPrev):(demote (l-1) cs)
    
    
    
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    deq :: Val -> Compt
    deq (Quot cp) = cp
    
    dmdq :: Val -> Compt
    dmdq = (demote 1).deq
    
    exp8 = (Next (Next (Add (Lit 2) (Lit 3))))
    
    run :: Compt -> Val
    run cp = appC4 cp [] Cont0
    run2 :: Compt -> Val
    run2 = run.deq.run
    run3 :: Compt -> Val
    run3 = run.deq.run2
    
    compile :: Term -> Compt
    compile = (flip eval4) 0
    
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
    
    
