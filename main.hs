module Main where

import System.Environment
import Control.Monad.Error
import Text.Parsec
import Data.Either
import LLVM.General.PrettyPrint
import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.PassManager
import LLVM.General.Transforms
import qualified Data.Text.IO as T
import Parser
import Codegen

main :: IO ()
main = do
    [filename] <- getArgs
    source <- T.readFile filename
    let p = parseProgram filename source
    ast <- case p of
        Right stmts -> return stmts
        Left err -> do
                print err
                fail "bad syntax"

    putStrLn "Success"
    mapM_ print ast
    let code = genCode ast
    putStrLn $ showPretty code
    putStrLn "Compiling"
    cret <- withContext $ \ctx ->
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
