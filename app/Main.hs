module Main where

--import Lib
import System.Environment
import Control.Monad
import Data.List

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (expr)

-- DICTIONARY
-- WORD
fWORD :: Char -> String -> (String, String)
fWORD delim inStr = (word, shortstr)
  where word     = takeWhile (/= delim) (inStr)
        shortstr = drop (1 + length word) inStr


-- note: stack is itself of type (,)
-- this creates a nested tuple of tuples
-- 

-- just an empty tuple
-- stack should be a Type!
stackInit = ((),)


fPUSH :: a -> b -> (a, b)
fPUSH top stack = (top, stack)

fPOP :: (a, b) -> (c, d)
fPOP top stack = case of stackEmpty in
  True -> putStrLn "Stack underflow"
  False -> (fst stack, snd stack)
  let stackEmpty = top == () --can't pop the top!
--unsafe version below
--fPOP _ stack = (fst stack, snd stack)