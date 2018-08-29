module FTypes where

-- State
-- -- Datastack
-- -- Compile Flag
-- -- Input String
-- -- Print String
-- -- Dictionary

data FState = FState
            { datastack     :: FDataStack
            , input_string  :: FInput
            , output_string :: FOutput
            , dictionary    :: FDict
            , compile_flag  :: FCFlag
            , program_count :: Int
            , return_stack  :: [Int]
            , exec_env      :: [[FStackItem]]
            , quit_flag     :: Bool -- replace with an exception monad?
            } deriving (Show)

-- Type aliases
type FInput = String
type FOutput = String
type FDataStack = [FStackItem]
type FDictEntry = (String, FIWord, FStackItem)
type FDict = [FDictEntry]
type FCFlag = Bool
type FCFn = [FStackItem]
type FList = [FStackItem]
type FNum = Int
type FStr = String
type FBool = Bool

data FIWord = Imm
            | Not
            | NA deriving (Show, Eq)

-- Stack content options
data FStackItem = FNum   Int
                | FStr   String
                | FFn    (FState -> FState)
                | FCFn   [FStackItem]
                | FIWord FIWord
                | FBool  Bool
                | FCFlag Bool
                | FList  [FStackItem]
                | FNull

instance Show FStackItem where
    show (FNum x)   = " #" ++ show x
    show (FStr [])  = " ~"
    show (FStr x)   = " ~" ++ x
    show (FFn  _)   = " <fn>"
    show (FCFn _)   = " [<fn>]"
    show (FIWord x) = " w" ++ show x
    show (FBool x)  = " b" ++ show x
    show (FCFlag x) = " f" ++ show x
    show (FList []) = " []"
    show (FList (x:[])) = show x
    show (FList x)  = show (head x) ++ show (tail x)
    show (FNull)    = " |null"
