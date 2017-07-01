module Dict where

import FTypes

-- helper functions
stack_op :: (FDataStack -> FDataStack) -> FState -> FState
stack_op f s = s { datastack = f (datastack s) }

stack_pop :: FDataStack -> FDataStack
stack_pop = drop 1

-- DICTIONARY

-- map of native functions
-- must manually add new native words here :/

native_dict = [ (".S"   ,FFn fDOTESS   )
              , ("."    ,FFn fDOT      )
              , ("BL"   ,FFn fBL       )
              , ("*"    ,FFn fSTAR     )
              , ("+"    ,FFn fADD      )
              , ("-"    ,FFn fSUB      )
              , ("/"    ,FFn fDIV      )
              , ("MAX"  ,FFn fMAX      )
              , ("MIN"  ,FFn fMIN      )
              , ("DUP"  ,FFn fDUP      )
              , ("DROP" ,FFn fDROP     )
              , ("SWAP" ,FFn fSWAP     )
              , ("OVER" ,FFn fOVER     )
              , ("ROT"  ,FFn fROT      )
              , ("NIP"  ,FFn fNIP      )
              , ("TUCK" ,FFn fTUCK     )
              , ("SQUARED",FCFn fSQUARED)
              ]

-- printing
fDOTESS :: FState -> FState
fDOTESS s = s { output_string =
                    "<len: "
                 ++ (show $ length (datastack s))
                 ++ "> "
                 ++ (show (datastack s))
                 ++ " nice stack =]" }

fDOT :: FState -> FState
fDOT s@(FState {datastack=[]}) =
        s { output_string = "stack's empty mate" }
fDOT s = s { datastack = stack_pop( datastack s  )
           , output_string = getPancake( datastack s )
           }
         where getPancake (cake:cakes) = show(cake) ++ " pancake!"



-- constants
fBL :: FState -> FState
fBL = stack_op(FStr " " :)



-- arithmetic
fSTAR :: FState -> FState
fSTAR = stack_op(dStar)
    where dStar (FNum s: FNum st:stk) = FNum(s * st) : stk

fADD :: FState -> FState
fADD = stack_op(dAdd)
    where dAdd (FNum s: FNum st:stk) = FNum(s + st) : stk

fSUB :: FState -> FState
fSUB = stack_op(dSub)
    where dSub (FNum s: FNum st:stk) = FNum(s - st) : stk

fDIV :: FState -> FState
fDIV = stack_op(dDiv)
    where dDiv (FNum s: FNum st:stk) = FNum(div s st) : stk

fMAX :: FState -> FState
fMAX = stack_op(dMax)
    where dMax (FNum s: FNum st:stk) = FNum(max s st) : stk

fMIN :: FState -> FState
fMIN = stack_op(dMin)
    where dMin (FNum s: FNum st:stk) = FNum(min s st) : stk



-- stack ops
fDUP :: FState -> FState
fDUP = stack_op(dDup)
    where dDup []        = []
          dDup (tos:stk) = tos:tos:stk

fDROP :: FState -> FState
fDROP = stack_op(dDrop)
    where dDrop []      = []
          dDrop (_:stk) = stk


fSWAP :: FState -> FState
fSWAP = stack_op(dSwap)
    where dSwap []            = []
          dSwap (tos:[])      = tos:[]
          dSwap (tos:nxt:stk) = nxt:tos:stk

fOVER :: FState -> FState
fOVER = stack_op(dOver)
    where dOver []            = []
          dOver (tos:[])      = tos:[]
          dOver (tos:nxt:stk) = nxt:tos:nxt:stk

fROT :: FState -> FState
fROT = stack_op(dRot)
    where dRot []                = []
          dRot (tos:[])          = tos:[]
          dRot (tos:nxt:[])      = tos:nxt:[]
          dRot (tos:nxt:thd:stk) = thd:tos:nxt:stk

fNIP :: FState -> FState
fNIP = stack_op(dNip)
    where dNip []          = []
          dNip (tos:[])    = tos: []
          dNip (tos:_:stk) = tos:stk

fTUCK :: FState -> FState
fTUCK = stack_op(dTuck)
    where dTuck []            = []
          dTuck (tos:[])      = tos:tos:[]
          dTuck (tos:nxt:stk) = tos:nxt:tos:stk

--COMPOSITE WORDS
--hand compiled

fSQUARED :: [FStackItem]
fSQUARED = [FFn fDUP, FFn fSTAR]
