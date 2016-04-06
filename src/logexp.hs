module Logexp where

import           Text.Megaparsec.Expr

default (Int, Float)

data Ident = Name String | Nr Int deriving (Show, Eq)

data Formula =
      Var Ident | Not Formula
     | And Formula Formula | Or Formula Formula
        deriving (Show,Eq)

toString :: Formula -> String
toString (Var identifier) = case identifier of
    Name s -> s
    Nr n -> "v" ++ show n
toString (Not formula) = case formula of
    Var _ -> "~" ++ toString formula
    _     -> "¬(" ++ toString formula ++ ")"
toString (And left right) =
    "(" ++ toString left ++ " A " ++ toString right ++ ")"
toString (Or left right) =
    "(" ++ toString left ++ " V " ++ toString right ++ ")"

-- Simple test formula below - REMOVE LATER
f = And (Var (Nr 1)) (Or (Var(Name "a")) (Not(Var(Name "b"))))

--
-- Parser for logical expressions
-- 
