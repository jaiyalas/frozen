# Simple

## eval0.hs -- eval only

  * language spec.

## eval1 -- eval + appK (part A)

  * applying defunctionalizing continuations

## eval2 -- eval + appK (part B)

  * applying currying transformation

## eval3 -- eval + appK + appC

  * applying defunctionalizing continuations (to appK)
  * introducing **Inst** as instructions of our VM

## eval4 -- eval + appK + appC

  * renamed constructors for **Cont** as middle language

# Expended

## feval0 -- eval only

  * language spec.
  * additional term: **Lit Int** (compare to eval0.hs)
  * added function **deq** and **step**  (compare to eval0.hs)

## feval1 -- eval + appK (part A)

  * applying defunctionalizing continuations
  * additional term: **Add Term Term**  (compare to eval0.hs)

## feval2 -- eval + appK (part B) 

  * applying currying transformation

## feval3 -- eval + appK + appC

  * applying defunctionalizing continuations (to appK)
  * introducing instruction, **Inst**, to model a VM
  * renamed constructors for **Inst** for readability 

## feval3 -- eval + appK + appC

  * renamed constructors for **Cont** as middle language
