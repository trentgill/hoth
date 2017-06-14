module Main where

--import Lib
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (expr)
  --(main shortstr)

--symbol :: Parser Char
--symbol = oneOf 

-- DICTIONARY
-- WORD
fWORD :: Char -> String -> (String, String)
fWORD delim inStr = (word, shortstr)
  where word     = takeWhile (/= delim) (inStr)
        shortstr = drop (1 + length word) inStr


