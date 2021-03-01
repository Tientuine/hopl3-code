{-
 -  HOPL/Types.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides type definitions used throughout other sources.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.Types where

-- Represent identifiers using a Haskell String
type Id = String

-- Source code is simply a text string
type Source = String

