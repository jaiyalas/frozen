module Feval4 where
    
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
              deriving Show
    
    data Cont = Helf                       -- 0:Lit 0:Var 0:Abs l:Lit l:Var 
              | Quote Cont                  -- 0:Next 
              | Unquote Cont                  -- 0:Prev 
              | EvArg Compt Env Cont         -- 0:App
              | EvBody Compt Env Cont         -- 0:App
              | EvAddFst Compt Env Cont         -- 0:Add
              | EvAddSnd Compt Cont             -- 0:Add
              | QAbs Cont                  -- l:Abs
              | QNext Cont                  -- l:Next
              | QPrev Cont                  -- l:Prev
              | QApp' Compt Int Env Cont    -- l:App
              | QApp Compt Cont            -- l:App
              | QAdd' Compt Int Env Cont    -- l:Add
              | QAdd Compt Cont            -- l:Add
    
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
    
    
    eval4 :: Term -> Compt
    eval4 (Lit n) = (Number n):[]
    eval4 (Var n) = (Access n):[]
    eval4 (Abs t) = (Lambda (eval4 t)):[]
    eval4 (Next t) = Enter:(eval4 t)
    eval4 (Prev t) = Leave:(eval4 t)
    eval4 (App t1 t2) = (Push (eval4 t2)):(eval4 t1)
    eval4 (Add t1 t2) = (Plus (eval4 t1)):(eval4 t2)
    
    
    appC4 :: Compt -> Int -> Env -> Cont -> Val
    appC4 [Number n] 0 e ct = appK4 ct (Val n) 
    appC4 [Number n] l e ct = appK4 ct (Quot [Number n]) 
    appC4 [Access n] 0 e ct = appK4 ct (e!!n)  
    appC4 [Access n] l e ct = appK4 ct (Quot [Access n]) 
    appC4 [Lambda cp] 0 e ct = appK4 ct (Clos cp e)  
    appC4 [Lambda cp] l e ct = appC4 cp l (shiftE l e) (QAbs ct) 
    appC4 (Enter:cp) 0 e ct = appC4 cp 1 e (Quote ct) 
    appC4 (Enter:cp) l e ct = appC4 cp (l+1) e (QNext ct) 
    appC4 (Leave:cp) 1 e ct = appC4 cp 0 e (Unquote ct)   
    appC4 (Leave:cp) l e ct = appC4 cp (l-1) e (QPrev ct)   
    appC4 ((Push cp2):cp1) 0 e ct = appC4 cp1 0 e (EvArg cp2 e ct)  
    appC4 ((Push cp2):cp1) l e ct = appC4 cp1 l e (QApp' cp2 l e ct)  
    appC4 ((Plus cp1):cp2) 0 e ct = appC4 cp1 0 e (EvAddFst cp2 e ct) 
    appC4 ((Plus cp1):cp2) l e ct = appC4 cp1 l e (QAdd' cp2 l e ct)  
    
    
    appK4 :: Cont -> Val -> Val
    appK4 (Helf) val = val
    appK4 (Quote ct) (Quot cp) = appK4 ct (Quot cp)
    appK4 (Unquote ct) (Quot cp) = appK4 ct (Quot cp)
    appK4 (Unquote ct) (Val n) = appK4 ct (Quot [Number n])
    appK4 (EvArg cp2 e ct) (Clos cp1' e') = appC4 cp2 0 e (EvBody cp1' e' ct)
    appK4 (EvBody cp1' e' ct) val = appC4 cp1' 0 (val:e') ct
    appK4 (EvAddFst cp2 e ct) (Val n) = appC4 cp2 0 e (EvAddSnd [Number n] ct)
    appK4 (EvAddSnd [Number n] ct) (Val m) = appK4 ct (Val (m + n)) 
    appK4 (QAbs ct) (Quot cp') = appK4 ct (Quot [Lambda cp'])
    appK4 (QNext ct) (Quot cp') = appK4 ct (Quot (Enter:cp'))
    appK4 (QPrev ct) (Quot cp') = appK4 ct (Quot (Leave:cp'))
    appK4 (QApp' cp2 l e ct) (Quot cp1') = appC4 cp2 l e (QApp cp1' ct)
    appK4 (QApp cp1' ct) (Quot cp2') = appK4 ct (Quot ((Push cp2'):cp1'))
    appK4 (QAdd' cp2 l e ct) (Quot cp1') = appC4 cp2 l e (QAdd cp1' ct)
    appK4 (QAdd cp1' ct) (Quot cp2') = appK4 ct (Quot ((Plus cp1'):cp2'))
    
    
    fig2 = Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0))))) (Next (Var 0)))))
    
    deq :: Val -> Compt
    deq (Quot cp) = cp
    
    run :: Compt -> Val
    run cp = appC4 cp 0 [] Helf
    run2 :: Compt -> Val
    run2 = run.deq.run
    run3 :: Compt -> Val
    run3 = run.deq.run2
    
    compile :: Term -> Compt
    compile = eval4
    
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
