{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Formula where

import           Prelude hiding (filter,map)
import           Data.Set(singleton,union,Set,filter,map)
import           Control.Applicative(empty)
import           Control.Monad(void)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

default (Int, Float)

data Ident = Name String | Nr Int deriving (Show, Eq, Ord)

data Formula =
      Var Ident | Not Formula
     | And Formula Formula | Or Formula Formula
        deriving (Show,Eq)

equiv :: Formula -> Formula -> Formula
equiv a b = And (Not( And a (Not b) )) (Not( And (Not a) b ))

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

toString :: Formula -> String
toString (Var identifier) = case identifier of
    Name s -> s
    Nr n -> "v" ++ show n
toString (Not formula) = case formula of
    Var _ -> "~" ++ toString formula
    _     -> "~(" ++ toString formula ++ ")"
toString (And left right) =
    "(" ++ toString left ++ " & " ++ toString right ++ ")"
toString (Or left right) =
    "(" ++ toString left ++ " | " ++ toString right ++ ")"





--
-- Parser for logical expressions
--
-- Lexer
sc :: Parser() -- NEEDED type annotation to allow inference for others!
sc =L.space (void spaceChar) empty empty -- "space consumer - no comments"

lexeme = L.lexeme sc
symbol = L.symbol sc

parens = between (symbol "(")  (symbol ")")

numIdent :: Parser Ident
numIdent = Nr . read <$> lexeme ( oneOf "v" *> some digitChar )
strIdent :: Parser Ident
strIdent = Name <$> lexeme ( some letterChar ) -- is this now sensitive to ordering?
--Parser
formula :: Parser Formula
formula = makeExprParser terms ops

ops :: [[Operator Parser Formula]]
ops = [ [ Prefix (symbol "~" *> pure Not)],
        [ InfixL (symbol "&" *> pure And)],
        [ InfixL (symbol "|" *> pure Or)] ]

terms :: Parser Formula
terms =  parens formula
     <|> try (Var <$> numIdent)
     <|> Var <$> strIdent

pf :: String -> Either ParseError Formula
pf = runParser formula ""
