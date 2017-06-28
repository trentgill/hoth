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
main = repl FState {datastack = [],
                    input_string = "",
                    output_string = "" } 

repl :: FState -> IO ()
repl state = do
    interpret_this <- getLine
    let inputState = state { input_string = interpret_this }
    let retState = fINTER inputState
    print (retState)
    repl retState 


fINTER :: FState -> FState
fINTER stay@(FState {input_string=[]}) = stay
fINTER stay = fINTER . fEXECUTE . fFIND . fWORD . fBL $ stay

stk_pop :: FDataStack -> FDataStack
stk_pop = drop 1

fWORD :: FState -> FState
fWORD s = s { datastack = (FStr word):(stk_pop $ datastack s)
            , input_string = str' }
    where word  = takeWhile (/= delim) (input_string s)
          str'  = drop (1 + length word) (input_string s)
          delim = getChar (datastack s)
          getChar (FStr c:stk) = head c

-- pattern match into the dictionary here!
fFIND :: FState -> FState
fFIND s = s { datastack = dFIND (datastack s) } where 
    dFIND []          = []
    dFIND (FStr s:ss)
            | s == "DUP"    = (FFn fDUP):ss
            | s == "*"      = (FFn fSTAR):ss
            | s == "SQUARED"= (FFn fSQUARED):ss
            | otherwise     = (FNum (toInteger (digitToInt $ head s))):ss
    dFIND _ = []
                -- nb: if toInteger fails
                    -- 'otherwise' should catch that
                    -- wildcard match if stack doesn't have str on top


fEXECUTE :: FState -> FState
fEXECUTE s@(FState {datastack=(FFn  xt:rest)}) = xt s {
    datastack = stk_pop $ datastack s}
fEXECUTE s@(FState {datastack=(FNum xt:rest)}) = s

--fDOLITERAL :: FPC -> FDataStack -> FDataStack
--fDOT
--fBYE

--fQUIT
--fACCEPT

--fWORD
--fFIND
--fIF, fELSE, fTHEN
--fQBRANCH, fBRANCH
--fEXECUTE
--fABORT

--fCOLON
--fSEMICOLON

--Return stack operations: R> >R R@
--Arithmetic/bitwise operations: MOD NEGATE OR AND INVERT XOR LSHIFT RSHIFT
--Simple math: ABS
--Comparators: = < <= > >= <>
--Memory access: ! @ +! MOVE FILL
--Console I/O: KEY EMIT


--LITERALS / CONSTANTS


--PRINTING
--fDOT :: FState -> FState
--fDOT s = s { datastack = dDot (datastack s) }
    --dDot 
    --where dDot []        = []
          --dDot (s:stk) = stk, print(s)

fDOT :: FDataStack -> FDataStack
fDOT []      = []
--fDOT (s:stk) = stk, print(s)


--COMPOSITE WORDS (hand compiled)
fSQUARED :: FState -> FState
fSQUARED = fDUP
       >>> fSTAR
