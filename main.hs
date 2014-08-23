module Main where

import System.Environment
import Control.Monad.Error
import Data.Either
import Foreign.Ptr
import LLVM.General.ExecutionEngine
import LLVM.General.PrettyPrint
import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.AST.Name
import qualified Data.Text.IO as T
import Parser
import Codegen

foreign import ccall "dynamic" kFunc :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = kFunc (castFunPtr fn :: FunPtr (IO Double))

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
            putStrLn "======= Unoptimized"
            mapM_ putStrLn (lines unopt)

            let passconf = defaultPassSetSpec {
                transforms = [
                    InstructionCombining,
                    Reassociate,
                    GlobalValueNumbering False,
                    SimplifyControlFlowGraph,
                    PromoteMemoryToRegister
                  ]
              }

            opt <- withPassManager passconf $ \pm -> do
                runPassManager pm m
                moduleLLVMAssembly m

            putStrLn "======= Optimized"
            mapM_ putStrLn (lines opt)

            withJIT ctx 0 $ \jit ->
--            withMCJIT ctx Nothing Nothing Nothing Nothing $ \jit ->
                withModuleInEngine jit m $ \e -> do
                    func <- getFunction e (Name "main")
                    case func of
                        Nothing -> putStrLn "main not found"
                        Just f -> do
                            putStrLn "Executing..."
                            res <- run f
                            putStrLn $ "Result " ++ show res

            return (unopt, opt)

    when (isLeft cret) $ let Left e = cret in fail $ "Compile error: " ++ e
