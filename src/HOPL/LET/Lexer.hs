{-
 -  HOPL/LET/Lexer.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the lexical specification for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops   = [ "-", "zero?" ]
    names = [ "let", "in", "if", "then", "else" ]
    style = emptyDef {
                Tok.commentLine = "#"
              , Tok.reservedOpNames = ops
              , Tok.reservedNames = names
              }

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

