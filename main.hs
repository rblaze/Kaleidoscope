module Main where

import Data.Attoparsec.ByteString
import Data.ByteString.Char8
import Parser

endParse :: Show r => Result r -> IO ()
endParse (Partial c) = endParse (c empty)
endParse r = print r

main :: IO ()
main = endParse $ parse parseExpr $ pack "1+2-(3+x)*5-f(a,b-2*3)*7" -- "1-2+3*4*5+6-7;"
