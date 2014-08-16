module Main where

import System.Environment
import Control.Monad.Error
import Data.Attoparsec.ByteString
import Data.Either
import LLVM.General.PrettyPrint
import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.PassManager
import LLVM.General.Transforms
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
    cret <- withContext $ \ctx -> do
        runErrorT $ withModuleFromAST ctx code $ \m -> do
            unopt <- moduleLLVMAssembly m
            let passconf = defaultPassSetSpec {
                transforms = [
                    InstructionCombining,
                    Reassociate,
                    GlobalValueNumbering False,
                    SimplifyControlFlowGraph
                  ]
              }
            withPassManager passconf $ \pm -> do
                runPassManager pm m
                opt <- moduleLLVMAssembly m
                return (unopt, opt)
    when (isLeft cret) $ let Left e = cret in fail $ "Compile error: " ++ e
    let Right (unoptasm, optasm) = cret
    putStrLn "======= Unoptimized"
    mapM_ putStrLn (lines unoptasm)
    putStrLn "======= Optimized"
    mapM_ putStrLn (lines optasm)
