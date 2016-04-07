module Formula where

import           Data.Set (Set, filter, map, singleton, union)
import           Prelude  hiding (all, any, filter, map, print)

default (Int, Float)

data Ident = Name String | Nr Int deriving (Eq, Ord)

instance Show Ident where
    show (Name string) = string
    show (Nr n) = "v" ++ show n

data Formula =
      Var Ident | Not Formula
    | And Formula Formula | Or Formula Formula
        deriving (Show,Eq)

impl :: Formula -> Formula -> Formula
impl a b = Not(And a (Not b))

equiv :: Formula -> Formula -> Formula
equiv a b = And (a `impl` b) (b `impl` a)

all :: [Formula] -> Formula
all [f] = f
all (f:fs) = And f (all fs)

any :: [Formula] -> Formula
any [f] = f
any (f:fs) = Or f (any fs)

identifiers :: Formula -> Set Ident
identifiers formula = case formula of
     Var id       -> singleton id
     Not(Var id)  -> singleton id
     And a b      -> identifiers a `union` identifiers b
     Or a b       -> identifiers a `union` identifiers b

varNumbers :: Formula -> Set Int
varNumbers formula =
    map (\(Nr n) -> n) $ filter numbered (identifiers formula) where
        numbered (Nr _)   = True
        numbered (Name _) = False

print :: Formula -> String
print (Var identifier) = case identifier of
    Name s -> s
    Nr n -> "v" ++ show n
print (Not formula) = case formula of
    Var _ -> "~" ++ print formula
    _     -> "~(" ++ print formula ++ ")"
print (And left right) =
    "(" ++ print left ++ " & " ++ print right ++ ")"
print (Or left right) =
    "(" ++ print left ++ " | " ++ print right ++ ")"

pr = print
