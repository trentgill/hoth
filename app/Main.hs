module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List
import Control.Arrow
import Data.Char
import Dict
import FTypes

main :: IO ()
main = repl FState { datastack = []
                   , input_string = ""
                   , output_string = ""
                   , dictionary = native_dict
                   , compile_flag = False
                   , return_stack = []
                   }

repl :: FState -> IO ()
repl state = do
    interpret_this <- getLine
    let inputState = state { input_string = interpret_this
                           , output_string = ""
                           }
    let retState = fQUIT inputState
    putStrLn (get_outstr retState)
    --print(retState)
    repl retState
    where
        get_outstr :: FState -> String
        get_outstr s@(FState {output_string=[]}) = "ok."
        get_outstr s = (output_string s)

--fDOLITERAL :: FPC -> FDataStack -> FDataStack

--fIF, fELSE, fTHEN
--fQBRANCH, fBRANCH
--fABORT

--Return stack operations: R> >R R@
--Arithmetic/bitwise operations: MOD NEGATE OR AND INVERT XOR LSHIFT RSHIFT
--Simple math: ABS
--Comparators: = < <= > >= <>
--Memory access: ! @ +! MOVE FILL
--Console I/O: KEY EMIT

