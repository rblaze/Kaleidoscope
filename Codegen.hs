module Codegen (genCode) where

import AST
import LLVM.General.AST
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant(Constant(Float))
import LLVM.General.AST.Float
import LLVM.General.AST.FloatingPointPredicate(FloatingPointPredicate(ULT))
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import Control.Monad.Writer
import Control.Monad.State
import Data.DList (DList, toList, singleton)
import Data.Word
import Text.Printf
import qualified Data.ByteString.Char8 as BS8

type CodeWriter = StateT Word (Writer (DList (Named Instruction)))

genCode :: [Statement] -> Module
genCode stmts = defaultModule {
                    moduleDefinitions = zipWith genStatement [0..] stmts
                  }

mkName :: BS8.ByteString -> Name
mkName = Name . BS8.unpack

genStatement :: Word -> Statement -> Definition
genStatement _ (SFunc fname args body) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = mkName fname,
        parameters = (map (\n -> Parameter double (mkName n) []) args, False),
        basicBlocks = [genBody body]
    }
genStatement _ (SExtern fname args) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = mkName fname,
        parameters = (map (\n -> Parameter double (mkName n) []) args, False)
    }
genStatement funcid (STopLevelExpr expr) = GlobalDefinition functionDefaults {
        returnType = double,
        name = UnName funcid,
        basicBlocks = [genBody expr]
    }

funcRet :: Named Terminator
funcRet = Do (Ret (Just (LocalReference double (Name "retval"))) [])

genBody :: Expr -> BasicBlock
genBody body = BasicBlock (Name "entry") code funcRet
    where
    code = toList $ execWriter $ evalStateT (genExpr "retval" body) 0


genBinOp :: String -> (Operand -> Operand -> [t] -> Instruction) -> Expr ->Expr -> CodeWriter ()
genBinOp outname op left right = do
    tmpid <- state $ \n -> (n , n + 1)
    let leftname = printf "left%d" tmpid
    let rightname = printf "right%d" tmpid
    genExpr leftname  left
    genExpr rightname right
    tell $ singleton $ (Name outname) := op (LocalReference double (Name leftname))
                                            (LocalReference double (Name rightname)) []

genExpr :: String -> Expr -> CodeWriter ()
genExpr outname (EConstant v) = tell $ singleton $ (Name outname) := Load False (ConstantOperand $ Float $ Double v) Nothing 0 []
genExpr outname (EVariable var) = tell $ singleton $ (Name outname) := Load False (LocalReference double (mkName var)) Nothing 0 []
genExpr outname (EBinOp '+' left right) = genBinOp outname (FAdd NoFastMathFlags) left right
genExpr outname (EBinOp '-' left right) = genBinOp outname (FSub NoFastMathFlags) left right
genExpr outname (EBinOp '*' left right) = genBinOp outname (FMul NoFastMathFlags) left right
genExpr outname (EBinOp '<' left right) = do
    tmpid <- state $ \n -> (n , n + 1)
    let tmpname = printf "cmp%d" tmpid
    genBinOp tmpname (FCmp ULT) left right
    tell $ singleton $ (Name outname) := UIToFP (LocalReference i1 (Name tmpname)) double []
genExpr _ (EBinOp op _ _) = fail $ "invalid binop " ++ show op
genExpr outname (ECall func args) = do
    baseid <- state $ \n -> (n, n + fromIntegral (length args))
    params <- zipWithM mkParam [baseid..] args
    tell $ singleton $ (Name outname) := Call False C [] (Right $ LocalReference double $ mkName func) params [] []
    where
    mkParam i expr = do
        let paramname = printf "param%d" i
        genExpr paramname expr
        return (LocalReference double $ Name paramname, [])
