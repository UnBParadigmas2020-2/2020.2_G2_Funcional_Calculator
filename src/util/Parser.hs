-- {-# LANGUAGE OverloadedStrings #-}

module Parser where
import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Data.Functor.Identity

envLang :: Token.LanguageDef ()
envLang = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.nestedComments  = False
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = []
  , Token.reservedOpNames = []
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser envLang

bracket :: Parser a -> Parser a
bracket = Token.parens lexer

operatorReserved :: String -> Parser ()
operatorReserved = Token.reservedOp lexer

operatorsTable :: Ex.OperatorTable String () Identity Expr
operatorsTable = [
    [ prefix "-" Neg ]
  , [ binary "^" Exp Ex.AssocRight ]
  , [ binary "*" Mul Ex.AssocLeft, binary "/" Div Ex.AssocLeft ]
  , [ binary "+" Add Ex.AssocLeft, binary "-" Sub Ex.AssocLeft ]
  ]

plus :: Parser String
plus = string "+"

minus :: Parser String
minus = string "-"

number :: Parser String
number = many1 digit

binary :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
binary name f = Ex.Infix (operatorReserved name >> return f)

prefix :: String -> (a -> a) -> Ex.Operator String () Identity a
prefix name f = Ex.Prefix (operatorReserved name >> return f)

postfix :: String -> (a -> a) -> Ex.Operator String () Identity a
postfix name f = Ex.Postfix (operatorReserved name >> return f)

numExpr :: Parser Expr
numExpr = do
  n <- double
  Token.whiteSpace lexer
  return (LNum n)

term :: Parser Expr
term =
      numExpr
  <|> bracket exprParser

unsignedDouble :: Parser String
unsignedDouble =
      try (do {char '.'; n <- number; return ("0." ++ n)}) -- .123 -> 0.123
  <|> try (do {n1 <- number; char '.'; n2 <- number; return (n1 ++ "." ++ n2)}) -- 1.23 -> 1.23
  <|> try number

double :: Parser Double
double = (do
  option "" plus
  m <- option "" minus
  n <- unsignedDouble
  return (read $ m ++ n)) <?> "number"

getContent :: Parser a -> Parser a
getContent p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

exprParser :: Parser Expr
exprParser = Ex.buildExpressionParser operatorsTable term

parser :: String -> Either ParseError Expr
parser = parse (getContent exprParser) "<stdin>"