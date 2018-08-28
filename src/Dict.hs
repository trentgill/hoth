module Dict where

import Text.Read --readMaybe
import FTypes

-- helper functions
-- this can/should use a functor ((a->b)->f a->f b)
--   where f represents all of FState other than FDataStack
stack_op :: (FDataStack -> FDataStack) -> FState -> FState
stack_op f s = s { datastack = f (datastack s) }

stack_pop :: FDataStack -> FDataStack
stack_pop = drop 1

output_append :: FOutput -> FState -> FState
output_append str s = s { output_string = (output_string s) ++ str }

-- DICTIONARY
-- to be added
-- FORGET (forgets all words defined since the named word)
-- ." (prints until it encounters ")
-- CR (prints a carriage return)
-- SPACE (prints a space)
-- SPACES (prints n spaces)
-- MOD (n1 n2 -- rem)
-- /MOD (n1 n2 -- rem quot)
-- INCLUDE (loads & interprets a text file from disk)
-- = < > 0=
-- INVERT (inverts a flag)
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

-- only words able to called at RUNTIME
native_dict = [ (".S"        ,Not, FFn  fDOTESS    )
              , ("."         ,Not, FFn  fDOT       )
              , ("WORDS"     ,Not, FFn  fWORDS     )
              , ("BL"        ,Not, FFn  fBL        )
              , ("WORD"      ,Not, FFn  fWORD      )
              , ("SEE"       ,Not, FFn  fSEE       )
              , ("\"\""      ,Not, FFn  fWORDtoSTK )
              , ("FIND"      ,Not, FFn  fFIND      )
              , ("["         ,Imm, FFn  fLEFTBRAK  )
              , ("]"         ,Not, FFn  fRITEBRAK  )
              , ("("         ,Imm, FFn  fPAREN     )
              , (":"         ,Not, FCFn fCOLON     )
              , (";"         ,Imm, FCFn fSEMIC     )
              , (","         ,Not, FFn  fCOMMA     )
              , ("IMMEDIATE" ,Not, FFn  fIMMEDIATE )
              , ("CREATE"    ,Not, FFn  fCREATE    )
              , ("DROP"      ,Not, FFn  fDROP      )
              , ("DOLIT"     ,Imm, FFn  fDOLIT     )
              , ("*"         ,Not, FFn  fSTAR      )
              , ("+"         ,Not, FFn  fADD       )
              , ("-"         ,Not, FFn  fSUB       )
              , ("/"         ,Not, FFn  fDIV       )
              , ("MAX"       ,Not, FFn  fMAX       )
              , ("MIN"       ,Not, FFn  fMIN       )
              , ("DUP"       ,Not, FFn  fDUP       )
              , ("?DUP"      ,Not, FFn  fQDUP      )
              , ("SWAP"      ,Not, FFn  fSWAP      )
              , ("ROT"       ,Not, FFn  fROT       )
              , (">0"        ,Not, FFn  fGTZ       )
              , ("<0"        ,Not, FFn  fLTZ       )
              , ("IF"        ,Imm, FCFn fIF        )
              , ("ELSE"      ,Imm, FCFn fELSE      )
              , ("THEN"      ,Imm, FCFn fTHEN      )
              , ("!"         ,Not, FFn  fSETMEM    )
              , ("HERE"      ,Not, FFn  fHERE      )
              , (">NUM"      ,Not, FFn  fTONUM     )
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
    where
        dSub (FNum s: FNum st:stk) = FNum(s - st) : stk
        dSub s = s

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
    where dDup []        = [] --error
          dDup (tos:stk) = tos:tos:stk

-- implement as FCFn -> T/F testing only happens in QBRANCH
fQDUP :: FState -> FState
fQDUP = stack_op(dQDup)
    where dQDup []             = [] --error
          dQDup (FNum n:stk)   = case n of
                                    0         -> FNum n:stk
                                    otherwise -> FNum n: FNum n:stk
          dQDup (FIWord i:stk) = case i of
                                    NA        -> FIWord i:stk
                                    otherwise -> FIWord i: FIWord i:stk

fDROP :: FState -> FState
fDROP = stack_op(dDrop)
    where dDrop []      = [] --error
          dDrop (_:stk) = stk

fSWAP :: FState -> FState
fSWAP = stack_op(dSwap)
    where dSwap []            = [] --error
          dSwap (tos:[])      = tos:[] --error
          dSwap (tos:nxt:stk) = nxt:tos:stk

fROT :: FState -> FState
fROT = stack_op(dRot)
    where dRot []                = [] --error
          dRot (tos:[])          = tos:[] --error
          dRot (tos:nxt:[])      = tos:nxt:[] --error
          dRot (tos:nxt:thd:stk) = thd:tos:nxt:stk

fLTZ :: FState -> FState
fLTZ = stack_op(dLTZ)
    where
        dLTZ [] = [] --error
        dLTZ (FNum n:stk)
            | n > 0     = FBool True : stk
            | otherwise = FBool False : stk
        dLTZ (FIWord i:stk) = case i of
                                  Not -> FBool True : stk
                                  otherwise -> FBool False : stk

fGTZ :: FState -> FState
fGTZ = stack_op(dGTZ)
    where
        dGTZ [] = [] --error
        dGTZ (FNum n:stk)
            | n < 0     = FBool True : stk
            | otherwise = FBool False : stk
        dGTZ (FIWord i:stk) = case i of
                                  Imm -> FBool True : stk
                                  otherwise -> FBool False : stk

--quit loop
-- add support for Maybe here where Nothing means ABORT
-- which part of FState should this wrap? DataStack?
fQUIT :: FState -> FState
fQUIT s@(FState {input_string = ""   }) = output_append("ok.\n") $ s
fQUIT s@(FState {compile_flag = True }) = fENTER fCOMPILE
                                       -- . output_append("<compile>\n")
                                        $ s
fQUIT s@(FState {compile_flag = False}) = fENTER fINTERPRET
                                       -- . output_append("<interpret>")
                                        $ s

fEXECUTE :: FState -> FState
fEXECUTE s@(FState {datastack = (FFn  x:_)}) = x
                                             . fDROP
                                            -- . output_append "x "
                                             $ s
fEXECUTE s@(FState {datastack = (FCFn x:_)}) = fENTER x
                                             . fDROP
                                            -- . output_append "| "
                                             $ s
fEXECUTE s = fABORT
           . output_append "!x "
           $ s -- can't execute

fTONUM :: FState -> FState
fTONUM = stack_op(isnum)
    where
        isnum :: FDataStack -> FDataStack
        isnum (FStr  "":xs) = xs                   -- ignore trailing whitespace?
        isnum (FStr " ":xs) = xs                   -- ignore trailing whitespace?
        isnum (FStr   x:xs) = case readMaybe x of
                                  Just num -> (FNum num) : xs
                                  Nothing  -> xs   -- TODO error!
        isnum (x:xs)        = xs -- error

fENTER :: [FStackItem] -> FState -> FState
fENTER fs s = composite fs
            $ s{ program_count = 0
               , exec_env      = fs : exec_env s
               , return_stack  = program_count s : return_stack s
               }
    where
        composite :: [FStackItem] -> FState -> FState
        composite fs s = case c_fin s fs of
                             True  -> s{ program_count = head $ return_stack s
                                       , exec_env      = tail $ exec_env s
                                       , return_stack  = tail $ return_stack s
                                       }
                             False -> case fs !\! (program_count s) of
                                          Just fn -> composite fs
                                                   . fMOVE_PC (Just (FNum 1))
                                                   . fEXECUTE
                                                   . stack_op(fn :)
                                                   $ s
                                          Nothing -> --output_append "!comp_match "
                                                     s
        c_fin :: FState -> [FStackItem] -> Bool
        c_fin s fs = (program_count s) == (length fs)

-- safeIndex, ie. "Maybe !!"
(!\!) :: [a] -> Int -> Maybe a
xs !\! ix
    | ix < length xs = Just (xs!!ix)
    | otherwise      = Nothing

fQBRANCH :: FState -> FState
fQBRANCH s@(FState {datastack = (FIWord NA:_)}) = fDROP
                                                . fMOVE_PC (fGetArg s)
                                               -- . output_append "qbr^ \n"
                                                $ s
fQBRANCH s@(FState {datastack = (FNum   0:_ )}) = fDROP
                                                . fMOVE_PC (fGetArg s)
                                               -- . output_append "qbr^ \n"
                                                $ s
fQBRANCH s@(FState {datastack = (FBool True:_ )}) = fDROP
                                                  . fMOVE_PC (fGetArg s)
                                                 -- . output_append "qbr^ \n"
                                                  $ s
fQBRANCH s                                      = fDROP
                                                . fMOVE_PC (Just (FNum 1))
                                               -- . output_append "qbr> \n"
                                                $ s -- skip arg

fBRANCH :: FState -> FState
fBRANCH s = fMOVE_PC (fGetArg s)
         -- . output_append "br \n"
          $ s

fGetArg :: FState -> Maybe FStackItem
fGetArg s = case (head $ exec_env s) !\! (1 + program_count s) of
                Just i  -> Just i
                Nothing -> Nothing

fMOVE_PC :: Maybe FStackItem -> FState -> FState
fMOVE_PC mi s =
    case mi of
        Just i  -> case unwrapNum i of
                       Just num -> s { program_count = num + program_count s }
                       Nothing  -> fABORT . output_append "!MVPC2 " $ s
        Nothing -> fABORT
                 . output_append "!MVPC "
                 $ s
    where
        unwrapNum :: FStackItem -> Maybe Int
        unwrapNum (FNum x) = Just x
        unwrapNum _        = Nothing

fDOLIT :: FState -> FState
fDOLIT s = fMOVE_PC (Just (FNum 1)) --skip literal
         . putLiteral (fGetArg s)
         $ s
    where
        putLiteral :: Maybe FStackItem -> FState -> FState
        putLiteral ms s = case ms of
                              Just st -> stack_op(st :) s
                              Nothing -> fABORT
                                       . output_append "!DOLIT"
                                       $ s

-- composite is ENTER
-- 151 is EXIT

-- consume input, drop fBL, add FStr string onto stack
fWORD :: FState -> FState
fWORD s = stack_op(FStr word :)
       -- . output_append ("\n\"" ++ word ++ "\"\n")
        . fDROP
        $ s { input_string = str' }
    where
        word  = takeWhile (/= delim)
              $ dropWhile (== delim)
              $ input_string s
        takeS = takeWhile (== delim) $ input_string s
        str'  = drop (1 + length word + length takeS)
                     (input_string s)
        delim = getChar (datastack s)
        getChar (FStr c:stk) = head c

fFindDictEntry :: String -> FDict -> Maybe FDictEntry
fFindDictEntry str d = let m = matchDictEntry d str
                       in case length m > 0 of
                              True  -> Just (head m)
                              False -> Nothing

matchDictEntry :: FDict -> String -> [FDictEntry]
matchDictEntry d str = [ (name, imm, fn) | (name, imm, fn) <- d
                                         , name == str ]

--matchDictByFn :: FDict -> FStackItem -> [FDictEntry]
--matchDictByFn d ffn = [ (name, imm, fn) | (name, imm, fn) <- d
--                                        , fn == ffn ]

fFIND :: FState -> FState
fFIND s = stack_op( dFIND (dictionary s) ) s
    where
        dFIND :: FDict -> FDataStack -> FDataStack
        dFIND _ []          = (FIWord NA):(FCFn []):[] -- this should be error'd
        dFIND d (FStr x:xs) = case fFindDictEntry x d of
                                  Nothing -> (FIWord NA):(FStr x):xs
                                  Just d  -> (FIWord $ getFlag d):(getDictFn d):xs
        getFlag :: FDictEntry -> FIWord
        getFlag (_,f,_) = f

getDictFn :: FDictEntry -> FStackItem
getDictFn (_,_,f) = f

getDictString :: FDictEntry -> String
getDictString (s,_,_) = s

fCOMMA :: FState -> FState
fCOMMA s@(FState {dictionary = (x:xs)}) =
    fDROP $ s { dictionary = (compileTo x):xs }
    where
        compileTo :: FDictEntry -> FDictEntry
        compileTo (st, f, FCFn x) =
            (st, f, FCFn (x ++ [newd s]))
        compileTo (st, f, FFn x) =
            (st, f, FCFn ([FFn x] ++ [newd s]))
        newd :: FState -> FStackItem
        newd = head . datastack

fDictEntryLength :: FState -> Int
fDictEntryLength s@(FState {dictionary = (x:xs)})
      = case makeFCFn . getDictFn $ x of
             Just fcfn -> length fcfn
             Nothing   -> -1
    where
        makeFCFn :: FStackItem -> Maybe FCFn
        makeFCFn (FCFn f) = Just f
        makeFCFn _        = Nothing

-- pushes &top-of-dict to datastack
-- NB: does *not* keep a reference to which word. assumes latest word
fHERE :: FState -> FState
fHERE s = stack_op((FNum . fDictEntryLength $ s) :) $ s

fTHEN :: FCFn
fTHEN = [ FFn fDUP
        , FFn fHERE
        , FFn fSUB
        , FFn fSWAP
        , FFn fSETMEM
        ]

-- !
fSETMEM :: FState -> FState
fSETMEM s@(FState {dictionary = (x:xs)}) = fDROP
                                         . fDROP
                                         $ s { dictionary = (setDictArg x):xs }
    where
        setDictArg :: FDictEntry -> FDictEntry
        setDictArg (name,flag,fn) =
            let (x,_:ys) = splitAt ((fAsFNum . fGetTOS $ s)) (fAsFCFn fn)
            in  (name,flag, FCFn (x ++ FNum (fAsFNum . fGetTOS2 $ s):ys))

-- unsafe type conversions! extend with Maybe's
fAsFNum :: FStackItem -> FNum
fAsFNum (FNum x) = x

fAsString :: FStackItem -> FStr
fAsString (FStr f) = f

fAsFCFn :: FStackItem -> FCFn
fAsFCFn (FCFn f) = f

fGetTOS :: FState -> FStackItem
fGetTOS s@(FState {datastack=(tos:stk)}) = tos

fGetTOS2 :: FState -> FStackItem
fGetTOS2 s@(FState {datastack=(tos:tos2:stk)}) = tos2
fGetTOS2 s = FNull --error

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
fPAREN = fDROP
       . fWORD
       . stack_op(FStr ")" :)

fWORDS :: FState -> FState
fWORDS s = output_append listo s
    where
        listo :: String
        listo = concat . addNL $ [ name | (name,_,_) <- dictionary s ]
        addNL :: [String] -> [String]
        addNL = map (++ "\n")

fWORDtoSTK :: FState -> FState
fWORDtoSTK = fWORD . fBL

-- : SEE ( -- word) ""
fSEE :: FState -> FState
fSEE s = fDROP
       . fPrintDictEntry       $ s
    where
        fPrintDictEntry :: FState -> FState
        fPrintDictEntry s = case fFindDictEntry (fAsString . fGetTOS $ s)
                                                (dictionary s) of
                                Just d  -> output_append ( fDecompile s
                                                         . getDictFn
                                                         $ d) s
                                Nothing -> s

fDecompile :: FState -> FStackItem -> String
fDecompile s (FCFn x) = concat $ map showStackItem x
    where
        showStackItem :: FStackItem -> String
--        showStackItem (FFn x) = getDictString
--                              . head
--                              $ matchDictByFn (dictionary s) (FFn x)
        showStackItem x       = show x
fDecompile _ x        = show x

-- quit
fBYE :: FState -> FState
fBYE s = s { quit_flag = True }

-- unimplemented
fABORT :: FState -> FState
fABORT = output_append "ABORT! "

--lists
--fBRACE :: FState -> FState
--fBRACE = stack_op(pList) . fWORD . stack_op(FStr "}" :)
--    where pList x:xs = newList : xs
--newList x =l


--COMPOSITE WORDS
--hand compiled

fINTERPRET :: FCFn
fINTERPRET = [ FFn fBL
             , FFn fWORD
             , FFn fFIND
             , FFn fQBRANCH , FNum 4
                , FFn fEXECUTE
             , FFn fBRANCH  , FNum 2
                , FFn fTONUM
             , FFn fQUIT -- main loop
             ]

fCOLON :: FCFn
fCOLON = [ FFn fBL
         , FFn fWORD
         , FFn fRITEBRAK
         , FFn fCREATE
         ]

fSEMIC :: FCFn
fSEMIC = [ FFn fLEFTBRAK ]

fCOMPILE :: FCFn
fCOMPILE = [ FFn fBL
           , FFn fWORD
           , FFn fFIND
           , FFn fQDUP
           , FFn fQBRANCH , FNum 10
                , FFn fGTZ
                , FFn fQBRANCH , FNum 4
                    , FFn fCOMMA
                , FFn fBRANCH , FNum 2
                    , FFn fEXECUTE
           , FFn fBRANCH , FNum 3
                , FFn fTONUM -- error check for non-num'able
                , FCFn fLITERAL
           , FFn fQUIT -- main loop
           ]

fLITERAL :: FCFn
fLITERAL = [ FFn fDOLIT , FFn fDOLIT , FFn fCOMMA
           , FFn fCOMMA
           ]

-- compiles QBRANCH and FNull (arg) to dictionary
-- leaves fHERE on datastack
fIF :: FCFn
fIF = [ FFn  fDOLIT , FFn fQBRANCH , FFn fCOMMA
      , FCFn fPOSTPONE             -- save place for QBRANCH arg
      ]

fELSE :: FCFn
fELSE = [ FFn  fDOLIT , FFn fBRANCH , FFn fCOMMA
        , FCFn fPOSTPONE           -- save place for BRANCH arg
        , FFn  fSWAP
        , FCFn fTHEN
        ]

fPOSTPONE :: FCFn
fPOSTPONE = [ FFn fHERE
            , FFn fDOLIT, FNull , FFn fCOMMA -- placeholder for arg
            ]
