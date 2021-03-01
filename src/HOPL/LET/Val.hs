{-
 -  HOPL/LET/Val.hs
 -
 -  Reference implementation of the toy language LET by Mitchell Wand.
 -  This module provides types for representing the values in LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Val where

-- Denoted values are any expressed value
type DenVal = ExpVal

-- Expressed values may be th result of an expression.
data ExpVal
    = NumVal  { expvalToNum  :: Integer }
    | BoolVal { expvalToBool :: Bool }
    deriving Eq

instance Show ExpVal where
    show (NumVal  n) = "(NumVal "  ++ show n ++ ")"
    show (BoolVal z) = "(BoolVal " ++ show z ++ ")"

