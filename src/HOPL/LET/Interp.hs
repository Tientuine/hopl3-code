{-
 -  HOPL/LET/Interp.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Interp (
    interp,interpWith,interpWith'
  ) where

import           Prelude hiding (exp)
import           Data.Either (fromRight)

import           HOPL.Types (Source)

import           HOPL.LET.Syntax
import           HOPL.LET.Environment
import           HOPL.LET.Parser
import           HOPL.LET.Val

{- top-level interpreter routines -}

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv

interpWith' :: Env -> Source -> ExpVal
interpWith' ρ = fromRight undefined . interpWith ρ

interpWith :: Env -> Source -> Either ParseError ExpVal
interpWith ρ src = flip valueOfProgram ρ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Env -> ExpVal
valueOfProgram (Pgm exp) ρ = valueOf exp ρ

{- semantic reductions for expressions -}

valueOf :: Exp -> Env -> ExpVal

valueOf (ConstExp n) _ = NumVal n

valueOf (VarExp x) ρ = applyEnv ρ x

valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ

valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

valueOf (LetExp x rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv ρ x v
    v  = valueOf rhs ρ

valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where exp' = case valueOf exp₁ ρ of
          BoolVal True  -> exp₂
          BoolVal False -> exp₃

