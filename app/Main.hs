module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List
import Data.Char
import Dict
import FTypes
import System.IO -- hSetBuffering

hoth_defs = ": SQ (     a -- a^2 ) DUP * ;                 "
         ++ ": CUBED (  a -- a^3 ) DUP DUP * * ;           "
         ++ ": NIP (  a b -- a ) SWAP DROP ;               "
         ++ ": TUCK ( a b -- b a b ) DUP ROT SWAP ;        "
         ++ ": OVER ( a b -- a b a ) SWAP TUCK ;           "
         ++ ": NEGATE ( a -- -a ) -1 * ;                   "
         ++ ": ABS  (   a -- |a| ) DUP <0 IF NEGATE THEN ; "

main :: IO ()
main = repl . fQUIT $ FState { datastack     = []
                             , input_string  = hoth_defs
                             , output_string = ""
                             , dictionary    = native_dict
                             , compile_flag  = False
                             , program_count = 0
                             , return_stack  = []
                             , exec_env      = []
                             , quit_flag     = False
                             }

--repl :: FState -> IO ()
--repl state = do
--    hSetBuffering stdout NoBuffering -- force putStr to exec before getLine
--    putStrLn (output_string state)
--    let newState = fQUIT
--                 . clearOutString
--                 $ state
--    if quit_flag newState
--        then return ()
--        else do
--            putStr "> "
--            accept_me <- getLine
--            repl . fACCEPT accept_me $ newState
--        where
--            fACCEPT string st = st { input_string = string }
--            clearOutString s = s { output_string = "" }


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

--fIF, fELSE, fTHEN
--fQBRANCH, fBRANCH
--fABORT

--Return stack operations: R> >R R@
--Arithmetic/bitwise operations: MOD NEGATE OR AND INVERT XOR LSHIFT RSHIFT
--Simple math: ABS
--Comparators: = < <= > >= <>
--Memory access: ! @ +! MOVE FILL
--Console I/O: KEY EMIT

