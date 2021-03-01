{-
 -  HOPL/LET/Parser.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the grammatical specification for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Parser (
    parseToplevel,ParseError
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import HOPL.LET.Lexer
import HOPL.LET.Syntax

int :: Parser Exp
int = do
    n <- integer
    return $ ConstExp n

variable :: Parser Exp
variable = do
    var <- identifier
    return $ VarExp var

iszero :: Parser Exp
iszero = do
    reservedOp "zero?"
    rand <- parens $ expr
    return $ IsZeroExp rand

difference :: Parser Exp
difference = do
    reservedOp "-"
    (rand₁:rand₂:_) <- parens $ commaSep expr
    return $ DiffExp rand₁ rand₂

letexp :: Parser Exp
letexp = do
    reserved "let"
    var <- identifier
    reserved "="
    rhs <- expr
    reserved "in"
    body <- expr
    return $ LetExp var rhs body

ifexp :: Parser Exp
ifexp = do
    reserved "if"
    test <- expr
    reserved "then"
    conseq <- expr
    reserved "else"
    altern <- expr
    return $ IfExp test conseq altern

expression :: Parser Exp
expression = Ex.buildExpressionParser [] expr

expr :: Parser Exp
expr = try int
    <|> try letexp
    <|> try ifexp
    <|> try difference
    <|> try iszero
    <|> variable

program :: Parser Pgm
program = do
  Pgm <$> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser Pgm
toplevel = do
    pgm <- program
    return pgm

parseExp :: String -> Either ParseError Exp
parseExp s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError Pgm
parseToplevel s = parse (contents toplevel) "<stdin>" s

