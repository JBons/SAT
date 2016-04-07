
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tseitin where

import           Formula

default (Int, Float)

cnf :: Formula -> Formula
cnf = id

step :: Formula -> [Formula] -> Int -> Int -> ([Formula], Int)
step (Or a b) cnfs nlast nthis =
    let newFormula = Var(Nr nthis) `equiv` Or (Var(Nr(nlast+1))) (Var(Nr(nlast+2))) in
    let (alist,alast) = step a [] (nlast+2) (nlast+1) in
    let (blist,blast) = step b [] alast (nlast+2) in
    (newFormula:alist++blist,blast)
step (And a b) cnfs nlast nthis =
    let newFormula = Var(Nr nthis) `equiv` And (Var(Nr(nlast+1))) (Var(Nr(nlast+2))) in
    let (alist,alast) = step a [] (nlast+2) (nlast+1) in
    let (blist,blast) = step b [] alast (nlast+2) in
    (newFormula:alist++blist,blast)
step (Not a) cnfs nlast nthis =
    let newFormula = Var(Nr nthis) `equiv` Not (Var(Nr(nlast+1)))  in
    let (alist,alast) = step a [] (nlast+2) (nlast+1) in
    (newFormula:alist, alast)
step v@(Var id) cnfs nlast nthis =
    let newFormula = v `equiv` Var(Nr nthis) in
    ([newFormula],nlast)   
