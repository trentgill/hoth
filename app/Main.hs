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
                   }

repl :: FState -> IO ()
repl state = do
    interpret_this <- getLine
    let inputState = state { input_string = interpret_this
                           , output_string = ""
                           }
    let retState = fINTER . addCubed $ inputState
    putStrLn (get_outstr retState)
    repl retState 


get_outstr :: FState -> String
get_outstr s@(FState {output_string=[]}) = "ok."
get_outstr s = (output_string s)

fINTER :: FState -> FState
fINTER stay@(FState {input_string=[]}) = stay
fINTER stay = fINTER . fEXECUTE . fFIND . fWORD . fBL $ stay

fEXECUTE :: FState -> FState
fEXECUTE s@(FState {datastack=(FNum x:xs)}) = s
fEXECUTE s@(FState {datastack=(FFn  x:xs)}) = x s {
    datastack = stack_pop $ datastack s}
fEXECUTE s@(FState {datastack=(FCFn x:rest)}) =
    comp_proc x s {datastack = stack_pop $ datastack s}
    where comp_proc :: [FStackItem] -> FState -> FState
          comp_proc ([])   st = st
          comp_proc (f:fs) st = comp_proc fs $ fEXECUTE st {
            datastack = f : datastack st}

--fCOLON :: FState -> FState
fCOLON = fLEFTBRAK . fWORD . fBL



--fCOLON s = s { dictionary = colonWord : dictionary s
--             , compile_flag = True }
--    where colonWord :: FDictEntry
--          colonWord = (newWord, FCFn [])
--          newWord s = fWORD . FBL $ s

--fDOLITERAL :: FPC -> FDataStack -> FDataStack

--fIF, fELSE, fTHEN
--fQBRANCH, fBRANCH
--fABORT

--fCOLON
--fSEMICOLON

--Return stack operations: R> >R R@
--Arithmetic/bitwise operations: MOD NEGATE OR AND INVERT XOR LSHIFT RSHIFT
--Simple math: ABS
--Comparators: = < <= > >= <>
--Memory access: ! @ +! MOVE FILL
--Console I/O: KEY EMIT

--example of colon word
-- : DIAGONAL?
--   @ DUP
--   2 11 23 WIN?
--   SWAP



