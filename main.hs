module Main where

import System.Environment
import Control.Monad.Error
import Data.Attoparsec.ByteString
import LLVM.General.PrettyPrint
import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.Module
import qualified Data.ByteString.Char8 as BS8
import Parser
import Codegen

endParse :: Result r -> Result r
endParse (Partial c) = endParse (c BS8.empty)
endParse p = p

main :: IO ()
main = do
    [filename] <- getArgs
    source <- BS8.readFile filename
    let p = endParse $ parse parseProgram source
    ast <- case p of
        (Done rest stmts) | BS8.null rest -> return stmts
        _ -> do
                print p
                fail "bad syntax"

    putStrLn "Success"
    mapM_ print ast
    let code = genCode ast
    putStrLn $ showPretty code
    putStrLn "Compiling"
    err <- withContext $ \ctx -> do
        runErrorT $ withModuleFromAST ctx code $ \m -> do
            moduleLLVMAssembly m
    case err of
        Left e -> putStrLn $ "Compile error: " ++ e
        Right asm -> mapM_ putStrLn (lines asm)
