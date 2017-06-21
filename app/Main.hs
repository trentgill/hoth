module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List

main :: IO ()
main = do
    interpret_this <- getLine
    print(fINTERPRET interpret_this [])
    main
    


    --print (fSQUARED      -- 49
    --     . fSEVEN        -- 7
    --     $ [] :: FStack) -- empty stack
    ----putStrLn (name)

-- TYPES
-- Forth State
    -- Data Stack
    -- Return Stack
    -- Program Counter
    -- Compile Flag
    -- Input string
    -- Print string
    -- Dictionary(?)

-- empty stack & string to be interp'd
-- initState = FState [] "5 SQUARED BYE"

--data FState =
--    FState { stack :: FStack
--           , instr :: FInput }

-- Input String (type alias)
type FInput = String


-- Data Stack
-- type alias, not an ADT
type FStack = [FStackItem]

-- define all the things that can be elements of the stack
data FStackItem = FNum Integer
                | FStr String
                deriving (Eq, Ord, Read)

instance Show FStackItem where
    show (FNum x) = show x
    show (FStr x) = x



-- DICTIONARY

--interpret loop in haskell is different to forth
--input is the input string from the repl
--output is 'printable' as returned by functions
--for now we just return the FStack and print the contents
--later implement DOT and DOTESS to change the 'printable' state in FState

--nb: this should take FState input for persistance across repl cycles
--currently, each line operates in isolation
--need the StateT monad

fINTERPRET :: String -> FStack -> FStack
fINTERPRET []  stk = stk -- base case, return stack state
fINTERPRET str stk = fINTERPRET str' $ fIPTWO (FStr word):stk
    where word = takeWhile (/= ' ') (str)
          str' = drop (1 + length word) str

-- interpret without any side-effects
fIPTWO :: FStack -> FStack
fIPTWO
--Interpret loop in Forth
--BL WORD FIND
--IF EXECUTE
--ELSE >TONUM
--THEN


--fWORD :: FStack

--fWORD :: FStack -> FStack
--fWORD (delim:inStr:st) = (word, shortstr)
  --where word           = takeWhile (/= delim) (inStr)
        --shortstr       = drop (1 + length word) inStr



--fDOLITERAL :: FPC -> FStack -> FStack
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


--exercise: implement these
--Return stack operations: R> >R R@
--Arithmetic/bitwise operations: MOD NEGATE OR AND INVERT XOR LSHIFT RSHIFT
--Simple math: ABS
--Comparators: = < <= > >= <>
--Memory access: ! @ +! MOVE FILL
--Console I/O: KEY EMIT

--LITERALS / CONSTANTS
fFIVE :: FStack -> FStack
fFIVE st = (FNum 5):st

fSEVEN :: FStack -> FStack
fSEVEN st = (FNum 7):st

fBL :: FStack -> FStack
fBL st = (FStr " "):st



--MATH FUNCTIONS
fSTAR :: FStack -> FStack
fSTAR (FNum s:FNum st:stk) = FNum(s * st) : stk

fADD :: FStack -> FStack
fADD (FNum s:FNum st:stk) = FNum(s + st) : stk

fSUB :: FStack -> FStack
fSUB (FNum s:FNum st:stk) = FNum(s - st) : stk

fDIV :: FStack -> FStack
fDIV (FNum s:FNum st:stk) = FNum(div s st) : stk

fMAX :: FStack -> FStack
fMAX (s:st:stk) = max s st : stk

fMIN :: FStack -> FStack
fMIN (s:st:stk) = min s st : stk



--PRINTING
fDOT :: FStack -> FStack
fDOT []      = []
--fDOT (s:stk) = stk, print(s)

fTONUM :: FStack -> FStack
fTONUM ((FStr s):st) = (read s):st
-- protect against non-numerals..



--STACK MODIFIERS
fDUP :: FStack -> FStack
fDUP []        = []
fDUP (tos:stk) = tos:tos:stk

fDROP :: FStack -> FStack
fDROP []      = []
fDROP (_:stk) = stk

fSWAP :: FStack -> FStack
fSWAP []            = []
fSWAP (tos:[])      = tos:[]
fSWAP (tos:nxt:stk) = nxt:tos:stk

fOVER :: FStack -> FStack
fOVER []            = []
fOVER (tos:[])      = tos:[]
fOVER (tos:nxt:stk) = nxt:tos:nxt:stk

fROT :: FStack -> FStack
fROT []                = []
fROT (tos:[])          = tos:[]
fROT (tos:nxt:[])      = tos:nxt:[]
fROT (tos:nxt:thd:stk) = thd:tos:nxt:stk

fNIP :: FStack -> FStack
fNIP []          = []
fNIP (tos:[])    = tos:[]
fNIP (tos:_:stk) = tos:stk

fTUCK :: FStack -> FStack
fTUCK []            = []
fTUCK (tos:[])      = tos:tos:[]
fTUCK (tos:nxt:stk) = tos:nxt:tos:stk



--COMPOSITE WORDS (hand compiled)
fSQUARED :: FStack -> FStack
fSQUARED stk = fSTAR . fDUP $ stk

--fINIT :: FStack -> FStack
--fINIT stk = fBYE
--          . fSQUARED
--          . 42
--          . DOLITERAL
--          $ stk

--fQDUP :: FStack -> FStack
--fQDUP ((FNum s):st)
--    | s /= 0 = s:s:st
--    | otherwise = s:st


