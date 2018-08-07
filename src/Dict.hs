module Dict where

import FTypes

-- helper functions
-- this can/should be an applicative
stack_op :: (FDataStack -> FDataStack) -> FState -> FState
stack_op f s = s { datastack = f (datastack s) }

stack_pop :: FDataStack -> FDataStack
stack_pop = drop 1

output_append :: FOutput -> FState -> FState
output_append str s = s { output_string = (output_string s) ++ str }

-- DICTIONARY

-- to be fixed / changed
-- WORD must discard leading whitespace

-- to be added
-- FORGET (forgets all words defined since the named word)
-- ." (prints until it encounters ")
-- CR (prints a carriage return)
-- SPACE (prints a space)
-- SPACES (prints n spaces)
-- MOD (n1 n2 -- rem)
-- /MOD (n1 n2 -- rem quot)
-- INCLUDE (loads & interprets a text file from disk)
-- = < > 0= 0< 0>
-- IF ELSE THEN
-- INVERT (inverts a flag)
-- ?DUP (dupes top stack value only if it's a TRUE flag)
-- ABORT" (if flag true, clear stack, print name of last interp'd word)
-- ?STACK (true if stack is EMPTY)
--
-- 1+
-- 1-
-- 2+
-- 2-
-- 2* (left shift)
-- 2/ (right shift)
--
-- ABS
-- NEGATE
-- >R and R> (move from stack to rstack then back)
-- */ (multiplies then divides. for fractions)

-- map of native functions
-- must manually add new native words here :/

native_dict = [ (".S"        ,Not, FFn  fDOTESS    )
              , ("."         ,Not, FFn  fDOT       )
              , ("BL"        ,Not, FFn  fBL        )
              , ("WORD"      ,Not, FFn  fWORD      )
              , ("FIND"      ,Not, FFn  fFIND      )
              , ("["         ,Imm, FFn  fLEFTBRAK  )
              , ("]"         ,Not, FFn  fRITEBRAK  )
              , ("("         ,Imm, FFn  fPAREN     )
              , (":"         ,Not, FCFn fCOLON     )
              , (";"         ,Imm, FCFn fSEMIC     )
              , ("IMMEDIATE" ,Not, FFn  fIMMEDIATE )
              , ("CREATE"    ,Not, FFn  fCREATE    )
              , ("DROP"      ,Not, FFn  fDROP      )
              , ("*"         ,Not, FFn  fSTAR      )
              , ("+"         ,Not, FFn  fADD       )
              , ("-"         ,Not, FFn  fSUB       )
              , ("/"         ,Not, FFn  fDIV       )
              , ("MAX"       ,Not, FFn  fMAX       )
              , ("MIN"       ,Not, FFn  fMIN       )
              , ("DUP"       ,Not, FFn  fDUP       )
              , ("SWAP"      ,Not, FFn  fSWAP      )
              , ("ROT"       ,Not, FFn  fROT       )
              , ("ABORT"     ,Not, FFn  fABORT     )
              , ("BYE"       ,Not, FFn  fBYE       )
              ]

-- printing
fDOTESS :: FState -> FState
fDOTESS s@(FState {datastack=[]}) = output_append("stack's empty\n") s
fDOTESS s = output_append(  "<len: "
                         ++ (show $ length (datastack s))
                         ++ "> "
                         ++ (show (datastack s))
                         ++ " {"
                         ++ (show (compile_flag s))
                         ++ "} nice stack =]\n"
                         ) s

fDOT :: FState -> FState
fDOT s@(FState {datastack=[]}) = output_append("stack's empty\n") s
fDOT s = fDROP . output_append( getPancake( datastack s ) ) $ s
         where
             getPancake :: FDataStack -> String
             getPancake (cake:cakes) = show(cake) ++ " pancake!\n"


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

fROT :: FState -> FState
fROT = stack_op(dRot)
    where dRot []                = []
          dRot (tos:[])          = tos:[]
          dRot (tos:nxt:[])      = tos:nxt:[]
          dRot (tos:nxt:thd:stk) = thd:tos:nxt:stk

--quit loop
-- add support for Maybe here where Nothing means ABORT
-- which part of FState should this wrap? DataStack?
fQUIT :: FState -> FState
fQUIT s@(FState {input_string = []})    = output_append("ok.\n") $ s
fQUIT s@(FState {compile_flag = True }) = fQUIT . fCOMPILE $ s
fQUIT s@(FState {compile_flag = False}) = fQUIT . fINTERPRET $ s

--interpret and parse
fINTERPRET :: FState -> FState
fINTERPRET = fEXECUTE . fFIND . fWORD . fBL

-- EXECUTE expects a flag on the stack before the function
-- this apparently presents problems with composite functions
-- think about interaction with the return stack
fEXECUTE :: FState -> FState
fEXECUTE s@(FState {datastack = (FIWord NA:_:_)}) = stack_op(isnum) . fDROP $ s
    where
        isnum :: FDataStack -> FDataStack
        isnum (FStr "":xs) = xs -- ignore trailing whitespace
        isnum (FStr  x:xs) = (FNum (read x) : xs) -- need to account for read failure
fEXECUTE s@(FState {datastack = (_:FFn x:_)}) = x . fDROP . fDROP $ s
fEXECUTE s@(FState {datastack = (_:FCFn x:_)}) = composite x . fDROP . fDROP $ s
    where
        composite :: [FStackItem] -> FState -> FState
        composite ([])   st = st
        composite (f:fs) st = composite fs
                            . fEXECUTE
                            . stack_op(FIWord Not :)
                            . stack_op(f :)
                            $ st
fEXECUTE s = fABORT $ s -- can't execute

-- composite is ENTER
-- 151 is EXIT

fWORD :: FState -> FState
fWORD s = stack_op(FStr word :) . fDROP $ s { input_string = str' }
    where
        word  = takeWhile (/= delim)
              $ dropWhile (== delim)
              $ input_string s
        takeS = takeWhile (== delim) $ input_string s
        str'  = drop (1 + length word + length takeS)
                     (input_string s)
        delim = getChar (datastack s)
        getChar (FStr c:stk) = head c

-- nb: could be a stack op by sending (dict s) as arg
fFIND :: FState -> FState
fFIND s = s { datastack = dFIND (datastack s) (dictionary s) }
    where dFIND [] _          = (FIWord NA):(FCFn []):[]
          dFIND (FStr x:xs) d = (matchImm):(matchDict):xs
            where
                matchD :: [FStackItem]
                matchD = [ fn | (name, imm, fn) <- d
                              , name == x ]
                matchDict = case matchD of
                          []  -> FStr x          -- unmatched, echo string
                          fun -> head fun
                matchI :: [FIWord]
                matchI = [ imm | (name, imm, fn) <- d
                               , name == x ]
                matchImm = case matchI of
                           []  -> FIWord NA      -- not found
                           fon -> FIWord (head fon)

--compilation
fCOMPILE :: FState -> FState
fCOMPILE = fCEXE . fFIND . fWORD . fBL

fCEXE :: FState -> FState
fCEXE s@(FState {datastack = (FIWord Imm:_)}) = fEXECUTE $ s
fCEXE s@(FState {datastack = (_:FStr "":_)})  = fDROP . fDROP $ s
fCEXE s@(FState {datastack = (_:FStr x:_)})   = fCOMPILEN . fDROP $ s
-- need to gracefully fallback if (read x) doesn't work
fCEXE s                                       = fCOMPILEC . fDROP $ s

fCOMPILEC :: FState -> FState
fCOMPILEC s@(FState {dictionary = (x:xs)}) =
    fDROP $ s { dictionary = (compileTo x):xs }
    where
        compileTo :: FDictEntry -> FDictEntry
        compileTo (s, f, FCFn x) =
            (s, f, FCFn (x ++ [newd]))
        compileTo (s, f, FFn x) =
            (s, f, FCFn ([FFn x] ++ [newd]))
        newd = head $ datastack s

fCOMPILEN :: FState -> FState
fCOMPILEN s@(FState {dictionary = (x:xs)}) =
    fDROP $ s { dictionary = (compileTo x):xs }
    where
        compileTo :: FDictEntry -> FDictEntry
        compileTo (st, f, FCFn x) =
            (st, f, FCFn (x ++ [newd $ head $ datastack s]))
        compileTo (st, f, FFn x) =
            (st, f, FCFn ([FFn x] ++ [newd $ head $ datastack s]))
        newd :: FStackItem -> FStackItem
        newd (FStr x) = (FNum (read x))

fLEFTBRAK :: FState -> FState
fLEFTBRAK s = s { compile_flag = False }

fRITEBRAK :: FState -> FState
fRITEBRAK s = s { compile_flag = True }

fCREATE :: FState -> FState
fCREATE s@(FState {datastack = (FStr name:xs)}) =
    fDROP $ s { dictionary = coWord : dictionary s }
    where
        coWord :: FDictEntry
        coWord = ( name
                 , Not
                 , FCFn [] )

-- IMMEDIATE
fIMMEDIATE :: FState -> FState
fIMMEDIATE s@(FState {dictionary = (x:xs)}) =
    s { dictionary = (i_flag x):xs }
    where
        i_flag :: FDictEntry -> FDictEntry
        i_flag (n,_,fn) = (n,Imm,fn)

fPAREN :: FState -> FState
fPAREN = fDROP . fWORD . stack_op(FStr ")" :)


--lists
--fBRACE :: FState -> FState
--fBRACE = stack_op(pList) . fWORD . stack_op(FStr "}" :)
--    where pList x:xs = newList : xs
--newList x =l


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

fCOLON :: FCFn
fCOLON = [ FFn fBL
         , FFn fWORD
         , FFn fRITEBRAK
         , FFn fCREATE
         ]

fSEMIC :: FCFn
fSEMIC = [ FFn fLEFTBRAK ]

-- quit
fBYE :: FState -> FState
fBYE s = s { quit_flag = True }

-- unimplemented
fABORT :: FState -> FState
fABORT s = s
