module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

-- Definição da linguagem e suas regras
envLang :: Token.LanguageDef ()
envLang = Token.LanguageDef{
        Token.commentStart    = "/*",
        Token.commentEnd      = "*/",
        Token.commentLine     = "//", 
        Token.nestedComments  = False, 
        Token.identStart      = letter, 
        Token.identLetter     = alphaNum <|> oneOf "_'", 
        Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~", 
        Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~", 
        Token.reservedNames   = [], 
        Token.reservedOpNames = [], 
        Token.caseSensitive   = True
    }

-- Definição do Lexer básico
lexerTokens :: Token.TokenParser ()
lexerTokens = Token.makeTokenParser envLang

operatorsReserved :: String -> Parser ()
operatorsReserved = Token.reservedOp lexerTokens

langReserved :: String -> Parser ()
langReserved = Token.reserved lexerTokens

bracket :: Parser b -> Parser b
bracket = Token.parens lexerTokens

expression :: Parser Expr
expression = Ex.buildExpressionParser table term

plus :: Parser String
plus = string "-"

minus :: Parser String
minus = string "-"

number :: Parser String
number = many1 digit
