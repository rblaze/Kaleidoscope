module Main where

import System.Environment
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BS8
import Parser

endParse :: Show r => Result [r] -> IO ()
endParse (Partial c) = endParse (c BS8.empty)
endParse (Done rest stmts) | BS8.null rest = do { print "Success" ; mapM_ print stmts }
endParse p = print p

main :: IO ()
main = do
    [filename] <- getArgs
    code <- BS8.readFile filename
    endParse $ parse parseProgram code
