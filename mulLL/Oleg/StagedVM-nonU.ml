(* Deriving Non-uniform code-generating Staged VM *)
(* `Deriving Compilers and Virtual Machines for a Multi-Level language'
    Atsushi Igarashi and Masashi Iwaki
    APLAS 2007.

  The following derivation of the non-uniform code-generating staged VM
  is heavily based on Atsushi Igarashi-san's code for the uniform
  staged VM, which is located on his Web page.
*)


type term = Var of int | Abs of term | App of term * term
          | Next of term | Prev of term
;;

(* split-eval evaluator *)
module Eval = 
struct
  type value = Clos of env * term | Q of term
  and env = value list

  let rec shift (t, l, j) = match t with
      Var n -> if (n >= j) && l = 0 then Var (n+1) else Var n
    | Abs t0 -> Abs (shift (t0, l, j+1))
    | App (t0, t1) -> App (shift (t0, l, j), shift (t1, l, j))
    | Next t0 -> Next (shift (t0, l-1, j))
    | Prev t0 -> Prev (shift (t0, l+1, j))

  let rec shiftE (e, l) = match e with
      [] -> []
    | Clos (e', t) :: e -> Clos (e', t) :: shiftE (e, l)
    | Q t :: e -> Q (shift (t, l-1, 0)) :: shiftE (e, l)

  (* eval0 : term * env -> value *)
  let rec eval0 (t,e) = match t with
    | Var n -> List.nth e n
    | Abs t0 -> Clos (e, t0)
    | App (t0, t1) ->
	let (Clos (e', t')) = eval0 (t0, e) in 
	let v  = eval0 (t1, e) in eval0 (t', v :: e')
    | Next t0 -> 
	let Q v = evalQ (t0, 1, e) in Q v

  (* evalQ : term * int * env -> value *)
  and evalQ (t, l, e) = match t, l with
    | Var n, l -> Q (Var n)
    | Abs t0, l -> 
	let Q v = evalQ (t0, l, shiftE (e, l)) in Q (Abs v)
    | App (t0, t1), l ->
	let Q v0 = evalQ (t0, l, e) in
	let Q v1 = evalQ (t1, l, e) in Q (App (v0, v1))
    | Next t0, l -> let Q v = evalQ (t0, l+1, e) in Q (Next v)
    | Prev t0, 1 -> let Q v = eval0 (t0, e) in Q v
    | Prev t0, l -> (* l > 1 *)
	let Q v = evalQ (t0, l-1, e) in Q (Prev v)

  (* main : term -> value *)
  let main t = eval0 (t, [])

  let run t = let Q t = main t in main t
end;;

(*  Sample term from Figure 2 of the paper *)
let t = 
  Next (Abs (Prev (App (Abs (Next (Abs (Prev (Var 0)))),
		       Next (Var 0)))));;

(* Example of a 3-level term *)
(* next(fun x -> next(fun y -> (prev (prev (next x))) (prev (next y)))) *)
let t3 = 
  Next (Abs 
    (Next (Abs 
     (App (Prev (Prev (Next (Var 0))),
           Prev (Next (Var 0)))))));;

Eval.main t;;
Eval.main t3;;
(*
- : Eval.value =
Eval.Q (Abs (Next (Abs (App (Prev (Var 0), Prev (Next (Var 0)))))))
*)

let Eval.Clos ([],body) = Eval.run t3 in
  Eval.main (App(Abs body,Next(Abs (Var 0))));;
(*
- : Eval.value = Eval.Q (Abs (App (Abs (Var 0), Var 0)))
*)

module EvalCPS =  (*  CPS interpreter *)
struct
  type value = Clos of env * term | Q of term
  and env = value list
  type cont = value -> value

  let rec shift (t, l, j) = match t with
      Var n -> if (n >= j) && l = 0 then Var (n+1) else Var n
    | Abs t0 -> Abs (shift (t0, l, j+1))
    | App (t0, t1) -> App (shift (t0, l, j), shift (t1, l, j))
    | Next t0 -> Next (shift (t0, l-1, j))
    | Prev t0 -> Prev (shift (t0, l+1, j))

  let rec shiftE (e, l) = match e with
      [] -> []
    | Clos (e', t) :: e -> Clos (e', t) :: shiftE (e, l)
    | Q t :: e -> Q (shift (t, l-1, 0)) :: shiftE (e, l)

  (* eval0 : term * env * cont -> value *)
  let rec eval0 (t, e, k) = match t with
      | Var n -> k (List.nth e n)
      | Abs t0 -> k (Clos (e, t0))
      | App (t0, t1) ->
	  eval0 (t0, e, fun (Clos (e', t')) ->
	    eval0 (t1, e, fun v -> eval0 (t', v::e', k)))
      | Next t0 -> evalQ (t0, 1, e, fun (Q v) -> k (Q v))

  (* evalQ : term * lev * env * cont -> value *)
  and evalQ (t, l, e, k) = match t, l with
      | Var n, l -> k (Q (Var n))
      | Abs t0, l -> 
	  evalQ (t0, l, shiftE (e, l), fun (Q v) -> k (Q (Abs v)))
      | App (t0, t1), l ->
	  evalQ (t0, l, e, fun (Q v0) ->
	    evalQ (t1, l, e, fun (Q v1) -> k (Q (App (v0, v1)))))
      | Next t0, l -> evalQ (t0, l+1, e, fun (Q v) -> k (Q (Next v)))
      | Prev t0, 1 -> eval0 (t0, e, fun (Q v) -> k (Q v))
      | Prev t0, l -> (* l > 1 *)
	  evalQ (t0, l-1, e, fun (Q v) -> k (Q (Prev v)))

  (* main : term -> value *)
  let main t = eval0 (t, [], fun v -> v)
end;;

EvalCPS.main t;;


module EvalCEK = (* Defunctionalized interpreter (CEK machine) *)
struct
  type value = Clos of env * term | Q of term
  and env = value list

  type cont =
    | Cont0
    | Cont1 of term * env * cont
    | Cont2 of value * cont
    | Cont3 of cont
    | Cont4 of cont

    | Cont5 of cont
    | Cont6 of term * int * env * cont
    | Cont7 of term * cont
    | Cont8 of cont
    | Cont9 of cont

  let rec shift (t, l, j) = match t with
      Var n -> if (n >= j) && l = 0 then Var (n+1) else Var n
    | Abs t0 -> Abs (shift (t0, l, j+1))
    | App (t0, t1) -> App (shift (t0, l, j), shift (t1, l, j))
    | Next t0 -> Next (shift (t0, l-1, j))
    | Prev t0 -> Prev (shift (t0, l+1, j))

  let rec shiftE (e, l) = match e with
      [] -> []
    | Clos (e', t) :: e -> Clos (e', t) :: shiftE (e, l)
    | Q t :: e -> Q (shift (t, l-1, 0)) :: shiftE (e, l)

  (* applyK : cont * value -> value *)
  let rec applyK = function
      (Cont0, v) -> v
    | (Cont1 (t1, e, k), v) -> eval0 (t1, e, Cont2 (v, k))
    | (Cont2 (Clos (e', t'), k), v) -> eval0 (t', v::e', k)
    | (Cont3 k, Q v) -> applyK (k, Q v)
    | (Cont4 k, Q v) -> applyK (k, Q v)
    | (Cont5 k, Q v) -> applyK (k, Q (Abs v))
    | (Cont6 (t1, l, e, k), Q v0) -> evalQ (t1, l, e, Cont7 (v0, k))
    | (Cont7 (v0, k), Q v1) -> applyK (k, Q (App (v0, v1)))
    | (Cont8 k, Q v) -> applyK (k, Q (Next v))
    | (Cont9 k, Q v) -> applyK (k, Q (Prev v))

  (* eval0 : term * env * cont -> value *)
  and eval0 (t, e, k) = match t with
	Var n -> applyK (k, List.nth e n)
      | Abs t0 -> applyK (k, Clos (e, t0))
      | App (t0, t1) -> eval0 (t0, e, Cont1 (t1, e, k))
      | Next t0 -> evalQ (t0, 1, e, Cont3 k)

  (* eval : term * int * env * cont -> value *)
  and evalQ (t, l, e, k) = match t,l with
      | Var n, l -> applyK (k, Q (Var n))
      | Abs t0, l -> evalQ (t0, l, shiftE (e, l), Cont5 k)
      | App (t0, t1), l -> evalQ (t0, l, e, Cont6 (t1, l, e, k))
      | Next t0, l -> evalQ (t0, l+1, e, Cont8 k)
      | Prev t0, 1 -> eval0 (t0, e, Cont4 k)
      | Prev t0, l -> (* l > 1 *)
	  evalQ (t0, l-1, e, Cont9 k)

  (* main : term -> value *)
  let main2 t = eval0 (t, [], Cont0)
end;;

EvalCEK.main2 t;;


module EvalCurry = (* Curried version *)
struct
  type value = Clos of env * compt | Q of term
  and compt = env * cont -> value
  and env = value list
  and cont =
      | Cont0
      | Cont1 of compt * env * cont
      | Cont2 of value * cont
      | Cont3 of cont
      | Cont4 of cont

      | Cont5 of cont
      | Cont6 of compt * env * cont (* No need to carry a level! *)
      | Cont7 of term * cont
      | Cont8 of cont
      | Cont9 of cont

  let rec shift (t, l, j) = match t with
      Var n -> if (n >= j) && l = 0 then Var (n+1) else Var n
    | Abs t0 -> Abs (shift (t0, l, j+1))
    | App (t0, t1) -> App (shift (t0, l, j), shift (t1, l, j))
    | Next t0 -> Next (shift (t0, l-1, j))
    | Prev t0 -> Prev (shift (t0, l+1, j))

  let rec shiftE (e, l) = match e with
      [] -> []
    | Clos (e', t) :: e -> Clos (e', t) :: shiftE (e, l)
    | Q t :: e -> Q (shift (t, l-1, 0)) :: shiftE (e, l)

  (* eval0 : term -> env * cont -> value *)
  let rec eval0 t = match t with
    | Var n -> (fun (e, k) -> applyK (k, List.nth e n))
    | Abs t0 -> 
        let c0 = eval0 t0 in (fun (e, k) -> applyK (k, Clos (e, c0)))
    | App (t0, t1) -> 
	let c0 = eval0 t0 in
	let c1 = eval0 t1 in (fun (e, k) -> c0 (e, Cont1 (c1, e, k)))
    | Next t0 -> 
	let c0 = evalQ (t0, 1) in (fun (e, k) -> c0 (e, Cont3 k))

  (* evalQ : term * int -> env * cont -> value *)
  and evalQ (t, l) = match t, l with
    | Var n, l -> (fun (e, k) -> applyK (k, Q (Var n)))
    | Abs t0, l -> 
        let c0 = evalQ (t0, l) in (fun (e, k) -> c0 (shiftE (e, l), Cont5 k))
    | App (t0, t1), l -> 
	let c0 = evalQ (t0, l) in
	let c1 = evalQ (t1, l) in (fun (e, k) -> c0 (e, Cont6 (c1, e, k)))
    | Next t0, l ->
	let c0 = evalQ (t0, l+1) in (fun (e, k) -> c0 (e, Cont8 k))
    | Prev t0, 1 -> 
	let c0 = eval0 t0 in (fun (e, k) -> c0 (e, Cont4 k))
    | Prev t0, l ->
	let c0 = evalQ (t0, l-1) in (fun (e, k) -> c0 (e, Cont9 k))

  (* applyK : cont * value -> value *)
  and applyK = function
      (Cont0, v) -> v
    | (Cont1 (c1, e, k), v) -> c1 (e, Cont2 (v, k))
    | (Cont2 (Clos (e', c'), k), v) -> c' (v::e', k)
    | (Cont3 k, Q v) -> applyK (k, Q v)
    | (Cont4 k, Q v) -> applyK (k, Q v)
    | (Cont5 k, Q v) -> applyK (k, Q (Abs v))
    | (Cont6 (c1, e, k), Q v0) -> c1 (e, Cont7 (v0, k))
    | (Cont7 (v0, k), Q v1) -> applyK (k, Q (App (v0, v1)))
    | (Cont8 k, Q v) -> applyK (k, Q (Next v))
    | (Cont9 k, Q v) -> applyK (k, Q (Prev v))

  (* main : term -> value *)
  let main t = eval0 t ([], Cont0)
end;;

EvalCurry.main t;;


module EvalVM = (* compt is defunctionalized *)
struct 
  type value = Clos of env * compt | Q of term
  and compt = inst list
  and inst = 
        Access of int
      | Close of compt
      | Push of compt
      | Enter (* next 0 *)
      | Leave (* prev 1 *)
      | PushQAbs of int (* abs *)
      | PushQNext (* next l *)
      | PushQPrev (* prev l *)
  and env = value list
  and cont =
      Cont0
      | Cont1 of compt * env * cont
      | Cont2 of value * cont
      | Cont3 of cont
      | Cont4 of cont

      | Cont5 of cont
      | Cont6 of compt * env * cont
      | Cont7 of term * cont
      | Cont8 of cont
      | Cont9 of cont

  let rec shift (t, l, j) = match t with
      Var n -> if (n >= j) && l = 0 then Var (n+1) else Var n
    | Abs t0 -> Abs (shift (t0, l, j+1))
    | App (t0, t1) -> App (shift (t0, l, j), shift (t1, l, j))
    | Next t0 -> Next (shift (t0, l-1, j))
    | Prev t0 -> Prev (shift (t0, l+1, j))

  let rec shiftE (e, l) = match e with
      [] -> []
    | Clos (e', t) :: e -> Clos (e', t) :: shiftE (e, l)
    | Q t :: e -> Q (shift (t, l-1, 0)) :: shiftE (e, l)

  (* eval0 : term -> inst list *)
  let rec eval0 t = match t with
    | Var n -> [Access n]
    | Abs t0 -> let c0 = eval0 t0 in [Close c0]
    | App (t0, t1) -> 
        let c0 = eval0 t0 and c1 = eval0 t1 in Push c1::c0
    | Next t0 -> let c0 = evalQ (t0, 1) in Enter::c0

  (* evalQ : term * int -> inst list *)
  and evalQ (t, l) = match t, l with
    | Var n, l -> [Access n]
    | Abs t0, l -> PushQAbs l :: evalQ (t0, l)
    | App (t0, t1), l -> Push (evalQ (t1, l)) :: evalQ (t0, l)
    | Next t0, l -> PushQNext :: evalQ (t0, l+1)
    | Prev t0, 1 -> let c0 = eval0 t0 in Leave::c0
    | Prev t0, l -> PushQPrev :: evalQ (t0, l-1)

  (* applyK : cont * value -> value *)
  and applyK = function
      (Cont0, v) -> v
    | (Cont1 (c1, e, k), v) -> applyC (c1, e, Cont2 (v, k))
    | (Cont2 (Clos (e', c'), k), v) -> applyC (c', v::e', k)
    | (Cont4 k, Q v) -> applyKQ (k, Q v)  (* mode switch *)

  (* applyKQ : cont * value -> value *)
  and applyKQ = function
    | (Cont3 k, Q v) -> applyK (k, Q v)   (* mode switch *)
    | (Cont5 k, Q v) -> applyKQ (k, Q (Abs v))
    | (Cont6 (c1, e, k), Q v0) -> applyCQ (c1, e, Cont7 (v0, k))
    | (Cont7 (v0, k), Q v1) -> applyKQ (k, Q (App (v0, v1)))
    | (Cont8 k, Q v) -> applyKQ (k, Q (Next v))
    | (Cont9 k, Q v) -> applyKQ (k, Q (Prev v))

  (* applyC : compt * env * cont -> value *)
  and applyC = function
      (Access n   ::[], e, k) -> applyK (k, List.nth e n)
    | (Close c0   ::[], e, k) -> applyK (k, Clos (e, c0))
    | (Push c1    ::c0, e, k) -> applyC (c0, e, Cont1 (c1, e, k))
    | (Enter      ::c0, e, k) -> applyCQ (c0, e, Cont3 k) (* mode switch *)

  (* applyCQ : compt * env * cont -> value *)
  and applyCQ = function
    | (Access n   ::[], e, k) -> applyKQ (k, Q (Var n))
    | (Leave      ::c0, e, k) -> applyC (c0, e, Cont4 k) (* mode switch *)
    | (PushQAbs l ::c0, e, k) -> applyCQ (c0, shiftE (e, l), Cont5 k)
    | (Push c1::c0, e, k)     -> applyCQ (c0, e, Cont6 (c1, e, k))
    | (PushQNext  ::c0, e, k) -> applyCQ (c0, e, Cont8 k)
    | (PushQPrev  ::c0, e, k) -> applyCQ (c0, e, Cont9 k)

  (* main : term -> value *)
  let main t = applyC (eval0 t, [], Cont0)
end;;

EvalVM.main t;;


module EvalVM_LLGC = (* LLGC = Low-Level Code Generation *)
struct
  type value = Clos of env * compt | Q of compt
  and compt = inst list
  and inst = 
        Access of int
      | Close of compt
      | Push of compt
      | Enter (* next 0 *)
      | Leave (* prev 1 *)
      | PushQAbs of int (* abs *)
      | PushQNext (* next l *)
      | PushQPrev (* prev l *)
  and env = value list
  and cont =
      Cont0
      | Cont1 of compt * env * cont
      | Cont2 of value * cont
      | Cont3 of cont
      | Cont4 of cont

      | Cont5 of int * cont
      | Cont6 of compt * env * cont
      | Cont7 of compt * cont
      | Cont8 of cont
      | Cont9 of cont

  let rec shift_compt (c, l, j) = match c with
    | [] -> []
    | Access n :: rest -> 
	(if (n >= j) && l = 0 then Access (n+1) else Access n)::
	shift_compt (rest,l,j)
    | Close c0 :: rest -> 
	Close (shift_compt (c0, l, j+1))::shift_compt (rest,l,j)
    | PushQAbs n :: rest -> PushQAbs n :: shift_compt (rest, l, j+1)
    | Push c0 :: rest -> 
	Push (shift_compt (c0, l, j)) :: shift_compt (rest, l,j)
    | x::rest -> x::shift_compt(rest,l,j)

  let rec shiftE (e, l) = match e with
      [] -> []
    | Clos (e', t) :: e -> Clos (e', t) :: shiftE (e, l)
    | Q c :: e -> Q (shift_compt (c, l-1, 0)) :: shiftE (e, l)

  (* eval0 : term -> inst list *)
  let rec eval0 t = match t with
    | Var n -> [Access n]
    | Abs t0 -> let c0 = eval0 t0 in [Close c0]
    | App (t0, t1) -> 
        let c0 = eval0 t0 and c1 = eval0 t1 in Push c1::c0
    | Next t0 -> let c0 = evalQ (t0, 1) in Enter::c0

  (* evalQ : term * int -> inst list *)
  and evalQ (t, l) = match t, l with
    | Var n, l -> [Access n]
    | Abs t0, l -> PushQAbs l :: evalQ (t0, l)
    | App (t0, t1), l -> Push (evalQ (t1, l)) :: evalQ (t0, l)
    | Next t0, l -> PushQNext :: evalQ (t0, l+1)
    | Prev t0, 1 -> let c0 = eval0 t0 in Leave::c0
    | Prev t0, l -> PushQPrev :: evalQ (t0, l-1)

  (* applyK : cont * value -> value *)
  and applyK = function
      (Cont0, v) -> v
    | (Cont1 (c1, e, k), v) -> applyC (c1, e, Cont2 (v, k))
    | (Cont2 (Clos (e', c'), k), v) -> applyC (c', v::e', k)
    | (Cont4 k, Q v) -> applyKQ (k, Q v)  (* mode switch *)

  (* applyKQ : cont * value -> value *)
  and applyKQ = function
    | (Cont3 k, Q v) -> applyK (k, Q v)   (* mode switch *)
    | (Cont5 (l,k), Q v) -> applyKQ (k, Q (PushQAbs l:: v))
    | (Cont6 (c1, e, k), Q v0) -> applyCQ (c1, e, Cont7 (v0, k))
    | (Cont7 (v0, k), Q v1) -> applyKQ (k, Q (Push v1 :: v0))
    | (Cont8 k, Q v) -> applyKQ (k, Q (PushQNext:: v))
    | (Cont9 k, Q v) -> applyKQ (k, Q (PushQPrev:: v))

  (* applyC : compt * env * cont -> value *)
  and applyC = function
      (Access n   ::[], e, k) -> applyK (k, List.nth e n)
    | (Close c0   ::[], e, k) -> applyK (k, Clos (e, c0))
    | (Push c1    ::c0, e, k) -> applyC (c0, e, Cont1 (c1, e, k))
    | (Enter      ::c0, e, k) -> applyCQ (c0, e, Cont3 k) (* mode switch *)

  (* applyCQ : compt * env * cont -> value *)
  and applyCQ = function
    | (Access n   ::[], e, k) -> applyKQ (k, Q [Access n])
    | (Leave      ::c0, e, k) -> applyC (c0, e, Cont4 k) (* mode switch *)
    | (PushQAbs l ::c0, e, k) -> applyCQ (c0, shiftE (e, l), Cont5 (l,k))
    | (Push c1::c0, e, k)     -> applyCQ (c0, e, Cont6 (c1, e, k))
    | (PushQNext  ::c0, e, k) -> applyCQ (c0, e, Cont8 k)
    | (PushQPrev  ::c0, e, k) -> applyCQ (c0, e, Cont9 k)

  (* main : term -> value *)
  let main t = applyC (eval0 t, [], Cont0)

  (* Cf. demotion from Walid's POPL2003 *)
  let rec demote l = function
    | [] -> []
    | Access n:: rest  -> Access n::demote l rest
    | PushQAbs 1::rest -> 
	if l = 1 then [Close (demote l rest)]
	else failwith "levels out of sync; Abs l=1"  
    | PushQAbs n::rest -> 
	if l = n then PushQAbs (n-1) :: demote l rest
	else failwith "levels out of sync; Abs l>1"  
    | Push c :: rest -> Push (demote l c) :: demote l rest
    | PushQNext :: rest when l = 1 ->
	Enter :: demote (l+1) rest
    | PushQNext :: rest  when l > 1 ->
	PushQNext :: demote (l+1) rest
    | PushQPrev :: rest  when l = 2 ->
	Leave :: demote (l-1) rest
    | PushQPrev :: rest  when l > 2 ->
	PushQPrev :: demote (l-1) rest

  let run t = let Q insts = main t in demote 1 insts
  let runapp t x = 
    let Q insts = main t in 
    let op = demote 1 insts in
    applyC(Push (eval0 x)::op, [], Cont0)
end;;

EvalVM_LLGC.main t;;

EvalVM_LLGC.main t3;;
(*
- : EvalVM_LLGC.value =
EvalVM_LLGC.Q
 [EvalVM_LLGC.PushQAbs 1; EvalVM_LLGC.PushQNext; EvalVM_LLGC.PushQAbs 2;
  EvalVM_LLGC.Push
   [EvalVM_LLGC.PushQPrev; EvalVM_LLGC.PushQNext; EvalVM_LLGC.Access 0];
  EvalVM_LLGC.PushQPrev; EvalVM_LLGC.Access 0]
*)

EvalVM_LLGC.run t3;;
(*
- : EvalVM_LLGC.compt =
[EvalVM_LLGC.Close
  [EvalVM_LLGC.Enter; EvalVM_LLGC.PushQAbs 1;
   EvalVM_LLGC.Push
    [EvalVM_LLGC.Leave; EvalVM_LLGC.Enter; EvalVM_LLGC.Access 0];
   EvalVM_LLGC.Leave; EvalVM_LLGC.Access 0]]
*)

EvalVM_LLGC.runapp t3 (Next(Abs (Var 0)));;

(*
- : EvalVM_LLGC.value =
EvalVM_LLGC.Q
 [EvalVM_LLGC.PushQAbs 1; EvalVM_LLGC.Push [EvalVM_LLGC.Access 0];
  EvalVM_LLGC.PushQAbs 1; EvalVM_LLGC.Access 0]
*)

(* A bit of explanation.

The paper seems to argue that since the VM no longer has access to the
level (other than knowing if it is 0 or positive), it cannot generate
correct code. We observe however since the compiler _knew_ the correct
levels, the generated code correctly instructs the VM to switch to the
compilation or the interpretation mode. So, the compiled code will
execute correctly with respect to level separations. We aren't sure
that the code generated in the compilation mode contains the correct
switching instructions though. The language in the paper
does not define any facility to `run' the generated code. But one can
easily imagine such a facility, a run command. That run operation must
at least remove the Q constructor. It can also do other things --
including scanning of the code and adjusting it! In MetaOCaml, running
of the code involves the whole compilation. Thus we certainly can
allow `run' to take time to scan the instructions and adjust them. We
know the generated code has the complete information for such an
adjustment: the next and prev operations in the generated code are
present as they were. Speaking formally, such an adjustment
corresponds to a `demotion' operation, described in Walid's POPL2003
paper on environment classifiers (and also in our PEPM2008 paper).

I think one can describe this approach as follows: the uniform VM
knows exactly at which level it is in. We observe however that [almost
all the time] the VM only cares if the level is 0 or not. We can build
a VM that cares all of the time only about the level being 0 or not;
the price to pay is that the generated code might be slightly
incorrect, but fixable. We know that the code is fixable because Next
and Prev terms at non-zero levels are insert themselves into the code,
so by examining the generated code, we can recover the full
levels. One may say that my VM is the uniform VM where the level is
given not directly but encoded in the _context_, in the number of
emitted Prev and Next instructions in the code. In the uniform VM, it
is easy to run the generated code: we remove the outermost Q
constructor and immediately give the result to the VM. In my VM,
running is more complex: I have to scan the whole generated code
during the demotion phase, doing adjustments. I hope this additional
work may be acceptable: after all, the implementation of 'run' in
MetaOCaml is very complex and most time consuming.

The final staged VM needs fewer than 5*2 VM
instructions: some instructions are shared between the compilation and
interpretations modes. Corresponding to two modes, there are actually
two, mutually tail-recursive, VM.

I believe the result is general: all the VM instructions can be shared
across the two modes, with the obvious exception of Next and Prev. The
Close instructions needed duplication (PushQAbs): (i) for the sake of
proper deBruijn index adjustments (shifting); (ii) because of the the
different instruction format: Close takes the body as an argument
whereas in the case of PushQAbs, the body of the function is
implicit. These differences could be reconciled and so PushQAbs and
Close could be merged.

*)
