{-
 -  Main.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides routines for executables baesd on LET.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import HOPL.LET.Interp

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)

import System.Console.Haskeline
import System.Environment (getArgs)

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
        minput <- getInputLine "LET> "
        case minput of
            Nothing    -> outputStrLn "Goodbye."
            Just input -> unless (input == ":q") $
              liftIO (case interp input of
                Left  err -> print err
                Right val -> print val) >> loop

run :: IO ()
run = do
    args <- getArgs
    if null args
        then putStrLn "hopl3-run: Missing source file name"
        else do prog <- readFile $ head args
                case interp prog of
                  Left  err -> print err
                  Right val -> print val
                return ()

