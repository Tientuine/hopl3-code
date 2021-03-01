{-
 -  Spec.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides a test suite for the LEt interpreter.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import HOPL.LET.Environment
import HOPL.LET.Interp
import HOPL.LET.Parser
import HOPL.LET.Val

import Test.Tasty
import Test.Tasty.Hspec

import Control.Monad

main = do
  passSpec <- testSpec "Value tests" $
    forM_ valueTests $ \(name, prog, expected) ->
      it name $ interpWith' testEnv prog `shouldBe` expected
  excSpec <- testSpec "Exception tests" $
    forM_ errTests $ \(name, prog) ->
      it name $ print (interpWith testEnv prog) `shouldThrow` anyException
  errSpec <- testSpec "Error tests" $
    forM_ errTests $ \(name, prog) ->
      it name $ print (interpWith testEnv prog) `shouldThrow` anyErrorCall
  defaultMain
    (testGroup "All tests"
      [ passSpec
      , excSpec
      , errSpec
      ])
  where
    testEnv = extendEnv' emptyEnv [ ("i", NumVal 1)
                                  , ("v", NumVal 5)
                                  , ("x", NumVal 10) ]

{- Individual test cases -}

valueTests =
  [ ("positive-const", "11", NumVal 11)
  , ("negative-const", "-33", NumVal (-33))
  , ("simple-arith-1", "-(44,33)", NumVal 11)
  , ("nested-arith-left", "-(-(44,33),22)", NumVal (-11))
  , ("nested-arith-right", "-(55, -(22,11))", NumVal 44)
  , ("test-var-1", "x", NumVal 10)
  , ("test-var-2", "-(x,1)", NumVal 9)
  , ("test-var-3", "-(1,x)", NumVal (-9))
  , ("if-true", "if zero?(0) then 3 else 4", NumVal 3)
  , ("if-false", "if zero?(1) then 3 else 4", NumVal 4)
  , ("if-eval-test-true", "if zero?(-(11,11)) then 3 else 4", NumVal 3)
  , ("if-eval-test-false", "if zero?(-(11, 12)) then 3 else 4", NumVal 4)
  , ("if-eval-test-true-2", "if zero?(-(11, 11)) then 3 else foo", NumVal 3)
  , ("if-eval-test-false-2", "if zero?(-(11,12)) then foo else 4", NumVal 4)
  , ("simple-let-1", "let x = 3 in x", NumVal 3)
  , ("eval-let-body", "let x = 3 in -(x,1)", NumVal 2)
  , ("eval-let-rhs", "let x = -(4,1) in -(x,1)", NumVal 2)
  , ("simple-nested-let", "let x = 3 in let y = 4 in -(x,y)", NumVal (-1))
  , ("check-shadowing-in-body", "let x = 3 in let x = 4 in x", NumVal 4)
  , ("check-shadowing-in-rhs", "let x = 3 in let x = -(x,1) in x", NumVal 2)
  ]

excTests =
  [ ("no-bool-to-diff-1", "-(zero?(0),1)")
  , ("no-bool-to-diff-2", "-(1,zero?(0))")
  , ("no-int-to-if", "if 1 then 2 else 3")
  ]

errTests =
  [ ("test-unbound-var-1", "foo")
  , ("test-unbound-var-1", "-(x,foo)")
  ]

