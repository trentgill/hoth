module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List
import Control.Arrow
import Data.Char

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


-- TYPES
-- Forth State
    -- Data Stack
    -- Program Counter
    -- Compile Flag
    -- Input string
    -- Print string
    -- Dictionary(?)

data FState = FState
  { datastack :: FDataStack
  , input_string :: FInput
  , output_string :: FOutput }
  deriving (Show)

-- Type aliases
type FInput = String
type FOutput = String
type FDataStack = [FStackItem]

-- All potential stack contents (XT etc)
data FStackItem = FNum Integer
                | FStr String
                | FFn  (FState -> FState)

instance Show FStackItem where
    show (FNum x) = show x
    show (FStr x) = x
    show (FFn  x) = "<function>"

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

--pack/unpack datastack affecting words
stack_op :: (FDataStack -> FDataStack) -> FState -> FState
stack_op f s = s { datastack = f (datastack s) }

--LITERALS / CONSTANTS
fBL :: FState -> FState
fBL = stack_op(FStr " " :)

--MATH FUNCTIONS
fSTAR :: FState -> FState
fSTAR s = s { datastack = dStar (datastack s) }
    where dStar (FNum s:FNum st:stk) = FNum(s * st) : stk


fADD :: FDataStack -> FDataStack
fADD (FNum s:FNum st:stk) = FNum(s + st) : stk

fSUB :: FDataStack -> FDataStack
fSUB (FNum s:FNum st:stk) = FNum(s - st) : stk

fDIV :: FDataStack -> FDataStack
fDIV (FNum s:FNum st:stk) = FNum(div s st) : stk

fMAX :: FDataStack -> FDataStack
fMAX (FNum s:FNum st:stk) = FNum(max s st) : stk

fMIN :: FDataStack -> FDataStack
fMIN (FNum s:FNum st:stk) = FNum(min s st) : stk



--PRINTING
--fDOT :: FState -> FState
--fDOT s = s { datastack = dDot (datastack s) }
    --dDot 
    --where dDot []        = []
          --dDot (s:stk) = stk, print(s)

fDOT :: FDataStack -> FDataStack
fDOT []      = []
--fDOT (s:stk) = stk, print(s)


--STACK MODIFIERS
fDUP :: FState -> FState
fDUP s = s { datastack = dDup (datastack s) }
    where dDup []        = []
          dDup (tos:stk) = tos:tos:stk

fDROP :: FDataStack -> FDataStack
fDROP []      = []
fDROP (_:stk) = stk

fSWAP :: FDataStack -> FDataStack
fSWAP []            = []
fSWAP (tos:[])      = tos:[]
fSWAP (tos:nxt:stk) = nxt:tos:stk

fOVER :: FDataStack -> FDataStack
fOVER []            = []
fOVER (tos:[])      = tos:[]
fOVER (tos:nxt:stk) = nxt:tos:nxt:stk

fROT :: FDataStack -> FDataStack
fROT []                = []
fROT (tos:[])          = tos:[]
fROT (tos:nxt:[])      = tos:nxt:[]
fROT (tos:nxt:thd:stk) = thd:tos:nxt:stk

fNIP :: FDataStack -> FDataStack
fNIP []          = []
fNIP (tos:[])    = tos:[]
fNIP (tos:_:stk) = tos:stk

fTUCK :: FDataStack -> FDataStack
fTUCK []            = []
fTUCK (tos:[])      = tos:tos:[]
fTUCK (tos:nxt:stk) = tos:nxt:tos:stk



--COMPOSITE WORDS (hand compiled)
fSQUARED :: FState -> FState
fSQUARED = fDUP
       >>> fSTAR
