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


