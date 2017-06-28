module Dict where

import FTypes

-- helper functions
stack_op :: (FDataStack -> FDataStack) -> FState -> FState
stack_op f s = s { datastack = f (datastack s) }

-- DICTIONARY

-- constants
fBL :: FState -> FState
fBL = stack_op(FStr " " :)

-- stack ops
fDUP :: FState -> FState
fDUP = stack_op(dDup)
    where dDup []        = []
          dDup (tos:stk) = tos:tos:stk

--fDROP :: FState -> FState
--fDROP
