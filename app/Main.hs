module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List

main :: IO ()
main = do
    print (fSQUARED  -- 7,5
         . fSEVEN   -- 7
         $ [] :: FStack)
    --(expr:_) <- getArgs
    --putStrLn (expr)


-- TYPES
-- Forth State
    -- Data Stack
    -- Return Stack
    -- Program Counter
    -- Compile Flag
    -- Input string
    -- Dictionary(?)


-- Data Stack
-- define all the things that can be elements of the stack
data FStackItem = FNum Integer
                | FStr String
                deriving (Eq, Ord, Read)
-- type alias, not an ADT
type FStack = [FStackItem]

instance Show FStackItem where
    show (FNum x) = show x
    show (FStr x) = x

--instance Num FStackItem where
    --(*) (FNum x, FNum y) = x * y



-- DICTIONARY
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


--fQDUP :: FStack -> FStack
--fQDUP ((FNum s):st)
--    | s /= 0 = s:s:st
--    | otherwise = s:st


--fWORD :: FStack -> FStack
--fWORD (delim:inStr:st) = (word, shortstr)
  --where word           = takeWhile (/= delim) (inStr)
        --shortstr       = drop (1 + length word) inStr
