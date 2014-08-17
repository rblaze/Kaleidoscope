module Codegen (genCode) where

import AST
import LLVM.General.AST
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant(Constant(Float, GlobalReference))
import LLVM.General.AST.Float
import LLVM.General.AST.FloatingPointPredicate(FloatingPointPredicate(ULT, ONE))
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import Control.Monad.State
import Data.Word
import Text.Printf
import qualified Data.ByteString.Char8 as BS8
import qualified Data.DList as D

data CodegenState = CodegenState {
    csTempIndex :: Word,
    csBlockName :: Name,
    csBlockInstrs :: D.DList (Named Instruction),
    csCompleteBlocks :: D.DList BasicBlock
  }
  deriving Show
type CodeWriter = State CodegenState

addInstr :: Named Instruction -> CodeWriter ()
addInstr i = modify $ \s -> s{csBlockInstrs = csBlockInstrs s `D.snoc` i}

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
        basicBlocks = genBody body
    }
genStatement _ (SExtern fname args) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = mkName fname,
        parameters = (map (\n -> Parameter double (mkName n) []) args, False)
    }
genStatement funcid (STopLevelExpr expr) = GlobalDefinition functionDefaults {
        returnType = double,
        name = UnName funcid,
        basicBlocks = genBody expr
    }

genBody :: Expr -> [BasicBlock]
genBody body = flip evalState initState $ do
        ret <- genExpr "retval" body
        CodegenState _ blockName code blocks <- get
        let lastBlock = BasicBlock blockName (D.toList code) (Do $ Ret (Just ret) [])
        return $ D.toList (blocks `D.snoc` lastBlock)
    where
    initState = CodegenState 0 (Name "entry") D.empty D.empty
    

genBinOp :: String -> (Operand -> Operand -> [t] -> Instruction) -> Expr ->Expr -> CodeWriter Operand
genBinOp outname op left right = do
    tmpid <- state $ \s -> let n = csTempIndex s in (n , s{csTempIndex = n + 1})
    let leftname = printf "left%d" tmpid
    let rightname = printf "right%d" tmpid
    leftref <- genExpr leftname left
    rightref <- genExpr rightname right
    addInstr $ Name outname := op leftref rightref []
    return $ LocalReference double (Name outname)

genExpr :: String -> Expr -> CodeWriter Operand
genExpr _ (EConstant v) = return $ ConstantOperand $ Float $ Double v
genExpr _ (EVariable var) = return $ LocalReference double (mkName var)
genExpr outname (EBinOp '+' left right) = genBinOp outname (FAdd NoFastMathFlags) left right
genExpr outname (EBinOp '-' left right) = genBinOp outname (FSub NoFastMathFlags) left right
genExpr outname (EBinOp '*' left right) = genBinOp outname (FMul NoFastMathFlags) left right
genExpr outname (EBinOp '<' left right) = do
    tmpid <- state $ \s -> let n = csTempIndex s in (n , s{csTempIndex = n + 1})
    let tmpname = printf "cmp%d" tmpid
    genBinOp tmpname (FCmp ULT) left right
    addInstr $ Name outname := UIToFP (LocalReference i1 (Name tmpname)) double []
    return $ LocalReference double (Name outname)

genExpr _ (EBinOp op _ _) = fail $ "invalid binop " ++ show op

genExpr outname (ECall func args) = do
    baseid <- state $ \s -> let n = csTempIndex s in (n , s{csTempIndex = n + fromIntegral (length args)})
    params <- zipWithM mkParam [baseid..] args
    addInstr $ Name outname := Call False C [] (Right $ ConstantOperand $ GlobalReference double $ mkName func) params [] []
    return $ LocalReference double (Name outname)
    where
    mkParam i expr = do
        let paramname = printf "param%d" i
        ref <- genExpr paramname expr
        return (ref, [])

genExpr outname (EIf cond trueCase falseCase) = do
    tmpid <- state $ \s -> let n = csTempIndex s in (n , s{csTempIndex = n + 1})
    let condname = printf "cond%d" tmpid
    let condvalname = printf "condval%d" tmpid
    let thenid = printf "then%d" tmpid
    let thenvalname = printf "thenval%d" tmpid
    let elseid = printf "else%d" tmpid
    let elsevalname = printf "elseval%d" tmpid
    let contid = printf "ifend%d" tmpid
    condret <- genExpr condname cond
    addInstr $ Name condvalname := FCmp ONE (ConstantOperand $ Float $ Double 0) condret []
    st0 <- get
    let ifblock = BasicBlock (csBlockName st0) (D.toList $ csBlockInstrs st0) (Do $ CondBr (LocalReference double $ Name condvalname) (Name thenid) (Name elseid) [])
    put $ CodegenState (csTempIndex st0) (Name thenid) D.empty (csCompleteBlocks st0 `D.snoc` ifblock)

    thenret <- genExpr thenvalname trueCase
    st1 <- get
    let thenblock = BasicBlock (csBlockName st1) (D.toList $ csBlockInstrs st1) (Do $ Br (Name contid) [])
    put $ CodegenState (csTempIndex st1) (Name elseid) D.empty (csCompleteBlocks st1 `D.snoc` thenblock)

    elseret <- genExpr elsevalname falseCase
    st2 <- get
    let elseblock = BasicBlock (csBlockName st2) (D.toList $ csBlockInstrs st2) (Do $ Br (Name contid) [])
    put $ CodegenState (csTempIndex st2) (Name contid) D.empty (csCompleteBlocks st2 `D.snoc` elseblock)

    addInstr $ Name outname := Phi double [(thenret, csBlockName st1), (elseret, csBlockName st2)] []
    return $ LocalReference double (Name outname)
