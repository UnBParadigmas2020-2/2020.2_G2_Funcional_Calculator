module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

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