module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List
import Control.Arrow
import Data.Char
import Dict
import FTypes

native_dict = [ ("DUP"  ,FFn fDUP      )
              , ("*"    ,FFn fSTAR     )
              , (".S"   ,FFn fDOTESS   )
              , ("."    ,FFn fDOT      )
              ]

main :: IO ()
main = repl FState { datastack = []
                   , input_string = ""
                   , output_string = ""
                   , dictionary = native_dict
                   }

repl :: FState -> IO ()
repl state = do
    interpret_this <- getLine
    let inputState = state { input_string = interpret_this
                           , output_string = ""
                           }
    let retState = fINTER inputState
    putStrLn (get_outstr retState)
    repl retState 


get_outstr :: FState -> String
get_outstr s@(FState {output_string=[]}) = "ok."
get_outstr s = (output_string s)

fINTER :: FState -> FState
fINTER stay@(FState {input_string=[]}) = stay
fINTER stay = fINTER . fEXECUTE . fFIND . fWORD . fBL $ stay

fWORD :: FState -> FState
fWORD s = s { datastack = (FStr word):(stack_pop $ datastack s)
            , input_string = str' }
    where word  = takeWhile (/= delim) (input_string s)
          str'  = drop (1 + length word) (input_string s)
          delim = getChar (datastack s)
          getChar (FStr c:stk) = head c

-- pattern match into the dictionary here!
fFIND :: FState -> FState
fFIND s = s { datastack = dFIND (datastack s)(dictionary s) } where
    dFIND [] _              = []
    dFIND (FStr x:xs) d = (matchDict):xs
        where matchIt :: [FStackItem]
              matchIt = [ fn | (name, fn) <- d
                             , name == x ]
              matchDict = case matchIt of
                        []  -> FNum (read x)
                        fun -> head fun
-- (FNum (toInteger (digitToInt $ head x))):xs
--    dFIND (FStr x:xs) d
--            | x == "DUP"    = (FFn fDUP):xs
--            | x == "*"      = (FFn fSTAR):xs
--            | x == "SQUARED"= (FFn fSQUARED):xs
--            | x == ".S"     = (FFn fDOTESS):xs
--            | x == "."      = (FFn fDOT):xs
--            | otherwise     = (FNum (toInteger (digitToInt $ head x))):xs
--    dFIND _ _ = []
                -- nb: if toInteger fails
                    -- 'otherwise' should catch that
                    -- wildcard match if stack doesn't have str on top
-- `d` above is a dictionary
-- this is a list of tuples, with name & list of stack items (usually
-- functions)
--
--

fEXECUTE :: FState -> FState
fEXECUTE s@(FState {datastack=(FFn  xt:rest)}) = xt s {
    datastack = stack_pop $ datastack s}
fEXECUTE s@(FState {datastack=(FNum xt:rest)}) = s

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


--COMPOSITE WORDS (hand compiled)
fSQUARED :: FState -> FState
fSQUARED = fDUP
       >>> fSTAR
