module Formula.Models where

import           Data.List       (filter, groupBy)
import           Data.Map.Strict (Map, fromList, (!))
import           Data.Set        (toList)
import           Formula

type Assignment = Map Ident Bool

universe :: Formula -> [Assignment]
universe formula =
    let ids = toList $ identifiers formula in
    fmap fromList $ sequence $ groupBy (using fst) ( cartProd ids [True,False] )
    where
        cartProd xs ys = [ (x,y) | x <- xs, y <-ys ]
        using f x y = (==)(f x)(f y)

eval :: Formula -> Assignment -> Bool
eval formula assignment = case formula of
    And a b    -> eval a assignment && eval b assignment
    Or a b     -> eval a assignment || eval b assignment
    Not a      -> not (eval a assignment)
    Var ident  -> assignment ! ident

model :: Formula -> [Assignment]
model formula = filter (eval formula) (universe formula)

-- TO DO:
-- Function to check equisatisfiability, mapping common variables to each other.
