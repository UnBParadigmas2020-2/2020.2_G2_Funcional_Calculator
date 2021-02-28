module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
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

operatorsReserved :: Parser -> Parser ()
bracket = Token.operatorsReserved lexerTokens

langReserved :: String -> Parser ()
bracket = Token.langReserved lexerTokens

bracket :: Parser b -> Parser bracket
bracket = Token.bracket lexerTokens
