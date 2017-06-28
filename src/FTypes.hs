module FTypes where

-- State
-- -- Datastack
-- -- Compile Flag
-- -- Input String
-- -- Print String
-- -- Dictionary

data FState = FState
            { datastack :: FDataStack
            , input_string :: FInput
            , output_string :: FOutput
            } deriving (Show)

-- Type aliases
type FInput = String
type FOutput = String
type FDataStack = [FStackItem]

-- Stack content options
data FStackItem = FNum Integer
                | FStr String
                | FFn  (FState -> FState)

instance Show FStackItem where
    show (FNum x) = show x
    show (FStr x) = x
    show (FFn  x) = "<function>"

