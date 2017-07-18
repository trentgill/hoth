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

native_dict = [ (".S"   ,False, FFn fDOTESS   )
              , ("."    ,False, FFn fDOT      )
              , ("BL"   ,False, FFn fBL       )
              , ("*"    ,False, FFn fSTAR     )
              , ("+"    ,False, FFn fADD      )
              , ("-"    ,False, FFn fSUB      )
              , ("/"    ,False, FFn fDIV      )
              , ("MAX"  ,False, FFn fMAX      )
              , ("MIN"  ,False, FFn fMIN      )
              , ("DUP"  ,False, FFn fDUP      )
              , ("DROP" ,False, FFn fDROP     )
              , ("SWAP" ,False, FFn fSWAP     )
              , ("OVER" ,False, FFn fOVER     )
              , ("ROT"  ,False, FFn fROT      )
              , ("NIP"  ,False, FFn fNIP      )
              , ("TUCK" ,False, FFn fTUCK     )
              , ("WORD" ,False, FFn fWORD     )
              , ("FIND" ,False, FFn fFIND     )
              , ("["    ,False, FFn fLEFTBRAK )
              , ("]"    ,True,  FFn fRITEBRAK )
              , (":"    ,False, FCFn fCOLON   )
              , ("SQUARED",False, FCFn fSQUARED)
              , ("DBL"  ,False, FCFn [FFn fDUP, FFn fADD])
              ]

-- printing
fDOTESS :: FState -> FState
fDOTESS s@(FState {datastack=[]}) =
        s { output_string = output_string s
          ++ "stack's empty mate"
          ++ "\n" }
fDOTESS s = s { output_string = output_string s
                 ++ "<len: "
                 ++ (show $ length (datastack s))
                 ++ "> "
                 ++ (show (datastack s))
                 ++ " nice stack =]"
                 ++ "\n" }

fDOT :: FState -> FState
fDOT s@(FState {datastack=[]}) =
        s { output_string = "stack's empty mate" }
fDOT s = s { datastack = stack_pop( datastack s  )
           , output_string = output_string s ++ getPancake( datastack s ) ++ "\n"
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


--quit loop
fQUIT :: FState -> FState
fQUIT s@(FState {input_string = []}) = s
fQUIT s@(FState {compile_flag = True }) = fQUIT . fCOMPILE $ s
fQUIT s@(FState {compile_flag = False}) = fQUIT . fINTERPRET $ s


--interpret and parse
fINTERPRET :: FState -> FState
fINTERPRET = fEXECUTE . fFIND . fWORD . fBL

fEXECUTE :: FState -> FState
fEXECUTE s@(FState {datastack = (FNum x:xs)}) = s
fEXECUTE s@(FState {datastack = (FFn  x:xs)}) = x s {
    datastack = stack_pop $ datastack s }
fEXECUTE s@(FState {datastack = (FCFn x:xs)}) =
    composite x s {datastack = stack_pop $ datastack s}
    where composite :: [FStackItem] -> FState -> FState
          composite ([])   st = st
          composite (f:fs) st = composite fs $ fEXECUTE st {
            datastack = f : datastack st }

fWORD :: FState -> FState
fWORD s = s { datastack = (FStr word):(stack_pop $ datastack s)
            , input_string = str' }
    where word  = takeWhile (/= delim) (input_string s)
          str'  = drop (1 + length word) (input_string s)
          delim = getChar (datastack s)
          getChar (FStr c:stk) = head c

fFIND :: FState -> FState
fFIND s = s { datastack = dFIND (datastack s)(dictionary s)  }
    where dFIND [] _          = []
          dFIND (FStr x:xs) d = (matchDict):xs
            where matchIt :: [FStackItem]
                  matchIt = [ fn | (name, flag, fn) <- d
                                 , name == x ]
                  matchDict = case matchIt of
                            []  -> FNum (read x)
                            fun -> head fun

--compilation
fCOMPILE :: FState -> FState
fCOMPILE s = s

fLEFTBRAK :: FState -> FState
fLEFTBRAK s = s { compile_flag = False }

fRITEBRAK :: FState -> FState
fRITEBRAK s = s { compile_flag = True }

fCREATE :: FState -> FState
fCREATE s@(FState {datastack = (FStr name:xs)}) =
    s { dictionary = coWord : dictionary s
      , datastack  = stack_pop $ datastack s }
    where coWord :: FDictEntry
          coWord = ( name
                   , False
                   , FCFn [] )

-- IMMEDIATE
fPAREN :: FState -> FState
fPAREN = stack_op(stack_pop) . fWORD . stack_op(FStr ")" :)



--COMPOSITE WORDS
--hand compiled
--
--note the runtime compilation example of CUBED
--after compilation works, should create a transformer
--that does the cubWord elements to avoid repetition
--
--after that works, can refactor to allow words to be
--entered as text strings that use the compilation step
--to add them to dictionary at haskell-compile time


fCOLON :: [FStackItem]
fCOLON = [ FFn fBL
         , FFn fWORD
         , FFn fRITEBRAK
         , FFn fCREATE ]

fSQUARED :: [FStackItem]
fSQUARED = [ FFn fDUP
           , FFn fSTAR ]

--runtime compilation
addCubed :: FState -> FState
addCubed s = s { dictionary = cubWord : dictionary s }
    where cubWord :: FDictEntry
          cubWord = ( "CUBED"
                    , False
                    , FCFn [ FFn fDUP
                           , FFn fDUP
                           , FFn fSTAR
                           , FFn fSTAR])

