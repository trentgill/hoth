module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List
import Data.Char
import Dict
import FTypes
import System.IO -- hSetBuffering

main :: IO ()
main = do
        hoth_defs <- readFile "src/hoth_lib.fth"
        repl . fQUIT $ FState { datastack     = []
                             , input_string  = hoth_defs
                             , output_string = ""
                             , dictionary    = native_dict
                             , compile_flag  = False
                             , program_count = 0
                             , return_stack  = []
                             , exec_env      = []
                             , quit_flag     = False
                             }

repl :: FState -> IO ()
repl state = do
    hSetBuffering stdout NoBuffering -- force putStr to exec before getLine
    putStr "> "
    accept_me <- getLine
    let newState = fQUIT . fACCEPT accept_me $ state
    putStrLn (output_string newState)
    if quit_flag newState
        then return ()
        else repl . clearOutString $ newState
    where
        fACCEPT string st = st { input_string = string }
        clearOutString s = s { output_string = "" }

--fDOLITERAL :: FPC -> FDataStack -> FDataStack

--Return stack operations: R> >R R@
--Arithmetic/bitwise operations: MOD OR AND INVERT XOR LSHIFT RSHIFT
--Simple math: ABS
--Comparators: = < <= > >= <>
--Memory access: ! @ +! MOVE FILL
--Console I/O: KEY EMIT

