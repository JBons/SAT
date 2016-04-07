
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tseitin where

import           Formula
import           Data.Set(maxView)

default (Int, Float)

-- Tseitin transformation of a formula. Output is list of formulas
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
        let newFormula = Var(Nr nthis) `equiv` Or (Var(Nr(nlast+1))) (Var(Nr(nlast+2))) in
        let (alist,alast) = step a (nlast+2) (nlast+1) in
        let (blist,blast) = step b alast (nlast+2) in
        (newFormula:alist++blist,blast)
    step (And a b) nlast nthis =
        let newFormula = Var(Nr nthis) `equiv` And (Var(Nr(nlast+1))) (Var(Nr(nlast+2))) in
        let (alist,alast) = step a (nlast+2) (nlast+1) in
        let (blist,blast) = step b alast (nlast+2) in
        (newFormula:alist++blist,blast)
    step (Not a) nlast nthis =
        let newFormula = Var(Nr nthis) `equiv` Not (Var(Nr(nlast+1)))  in
        let (alist,alast) = step a (nlast+2) (nlast+1) in
        (newFormula:alist, alast)
    step v@(Var id) nlast nthis =
        let newFormula = v `equiv` Var(Nr nthis) in
        ([newFormula],nlast)

-- Helpers for next iteration
-- (vl ∨¬vx)∧(vr ∨¬vx)∧(¬vl ∨¬vr ∨vx)
cnfAnd x l r = [ Or l (Not x)
               , Or r (Not x)
               , Or x (Or (Not l)(Not r)) ]

--(¬vl ∨ vx) ∧ (¬vr ∨ vx) ∧ (vl ∨ vr ∨¬vx)
cnfOr x l r  = [ Or (Not l) x
               , Or (Not r) x
               , Or (Not x) (Or l r) ]

-- a <=> ~b == (a v ~b) (a v b) (~a v ~b)
cnfNot x s   = [ Or x (Not s)
               , Or x s
               , Or (Not x) (Not s) ]
