module Formula.Parser where

import           Control.Applicative    (empty)
import           Control.Monad          (void)
import           Data.Maybe             (fromJust)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           Formula

--
-- Parser for logical expressions - using the new Megaparsec package
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
ops = [ [ Prefix (symbol "~" *> pure Not)]
      , [ InfixL (symbol "&" *> pure And)]
      , [ InfixL (symbol "|" *> pure Or)]
      , [ InfixL (symbol "=" *> pure equiv), InfixL (symbol ">" *> pure impl)]
      ]

terms :: Parser Formula
terms =  parens formula
     <|> try (Var <$> numIdent)
     <|> Var <$> strIdent

safeParse :: String -> Either ParseError Formula
safeParse = runParser formula ""

pf :: String -> Formula
pf = fromJust.parseMaybe formula
