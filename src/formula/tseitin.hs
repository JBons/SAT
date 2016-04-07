module Formula.Tseitin where

import           Formula
import           Data.Set(maxView)

default (Int, Float)

-- NOTE: The transformation _should_ be correct, but it can be
-- optimised by removing redundant new variables for leaves.
-- To do once rest is working.

-- Tseitin transformation of a formula.
-- Output is list of disjunction formulas (only ORs and NOTs)
-- whose conjunction is equisatisfiable with the original formula.
-- Notice that the new variable corresponding to the root of the
-- formula parse tree is added separately to the list of formulas.
tseitin :: Formula -> [Formula]
tseitin formula = Var(Nr firstFree) : generated where
    generated = fst $ step formula firstFree firstFree

    firstFree :: Int
    firstFree = case maxView (varNumbers formula) of
        Just (n, _)  -> n+1
        Nothing      -> 0

    step :: Formula -> Int -> Int -> ([Formula], Int)
    step (Or a b) nlast nthis =
        let newFormulas = cnfOr (Var(Nr nthis)) (Var(Nr(nlast+1))) (Var(Nr(nlast+2))) in
        let (alist,alast) = step a (nlast+2) (nlast+1) in
        let (blist,blast) = step b alast (nlast+2) in
        (newFormulas ++ alist ++ blist, blast)
    step (And a b) nlast nthis =
        let newFormulas = cnfAnd (Var(Nr nthis)) (Var(Nr(nlast+1))) (Var(Nr(nlast+2))) in
        let (alist,alast) = step a (nlast+2) (nlast+1) in
        let (blist,blast) = step b alast (nlast+2) in
        (newFormulas ++ alist ++ blist, blast)
    step (Not a) nlast nthis =
        let newFormulas = cnfNot (Var(Nr nthis)) (Var(Nr(nlast+1)))  in
        let (alist,alast) = step a (nlast+2) (nlast+1) in
        (newFormulas ++ alist, alast)
    step v@(Var id) nlast nthis =
        let newFormulas = cfEq (Var(Nr nthis)) v in
        (newFormulas,nlast)

    -- (vl ∨¬vx)∧(vr ∨¬vx)∧(¬vl ∨¬vr ∨vx)
    cnfAnd x l r = [ Or l (Not x)
                   , Or r (Not x)
                   , Or x (Or (Not l)(Not r)) ]

    --(¬vl ∨ vx) ∧ (¬vr ∨ vx) ∧ (vl ∨ vr ∨¬vx)
    cnfOr x l r  = [ Or (Not l) x
                   , Or (Not r) x
                   , Or (Not x) (Or l r) ]

    -- a <=> ~b == (a v b) (~a v ~b)
    cnfNot x s   = [ Or x s , Or (Not x) (Not s) ]

    -- a <=> b == (a v ~b)(~a v b)
    cfEq x v   = [ Or x (Not v), Or (Not x) v ]
