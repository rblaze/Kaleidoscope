module Codegen (genCode) where

import AST
import LLVM.General.AST
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant(Constant(Float, GlobalReference))
import LLVM.General.AST.Float
import LLVM.General.AST.FloatingPointPredicate(FloatingPointPredicate(ULT, ONE))
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Word
import Text.Printf
import qualified Data.DList as D
import qualified Data.Map.Strict as M

data SymbolType = Param | AllocA
type SymbolTable = M.Map String SymbolType
data CodegenState = CodegenState {
    csTempIndex :: Word,
    csBlockName :: Name,
    csBlockInstrs :: D.DList (Named Instruction),
    csCompleteBlocks :: D.DList BasicBlock
  }
  deriving Show
type CodeWriter = ReaderT SymbolTable (State CodegenState)

addInstr :: Named Instruction -> CodeWriter ()
addInstr i = modify $ \s -> s{csBlockInstrs = csBlockInstrs s `D.snoc` i}

startNewBlock :: Named Terminator -> Name -> CodeWriter Name
startNewBlock term newBlockName = do
    CodegenState tempidx blockname code blocks <- get
    let block = BasicBlock blockname (D.toList code) term
    put $ CodegenState tempidx newBlockName D.empty (blocks `D.snoc` block)
    return blockname

reserveTempIndex :: Word -> CodeWriter Word
reserveTempIndex num = state $ \s -> let n = csTempIndex s
                                      in (n , s{csTempIndex = n + num})

genCode :: [Statement] -> Module
genCode stmts = defaultModule {
                    moduleDefinitions = zipWith genStatement [0..] stmts
                  }

genStatement :: Word -> Statement -> Definition
genStatement _ (SFunc fname args body) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = Name fname,
        parameters = (map (\n -> Parameter double (Name n) []) args, False),
        basicBlocks = genBody (M.fromList $ map (\a -> (a, Param)) args) body
    }
genStatement _ (SExtern fname args) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = Name fname,
        parameters = (map (\n -> Parameter double (Name n) []) args, False)
    }
genStatement funcid (STopLevelExpr expr) = GlobalDefinition functionDefaults {
        returnType = double,
        name = UnName funcid,
        basicBlocks = genBody M.empty expr
    }

genBody :: SymbolTable -> Expr -> [BasicBlock]
genBody syms body = flip evalState initState $ flip runReaderT syms $ do
        ret <- genExpr (Name "retval") body
        CodegenState _ blockName code blocks <- get
        let lastBlock = BasicBlock blockName (D.toList code) (Do $ Ret (Just ret) [])
        return $ D.toList (blocks `D.snoc` lastBlock)
    where
    initState = CodegenState 0 (Name "entry") D.empty D.empty
    
genBinOp :: Name -> (Operand -> Operand -> [t] -> Instruction) -> Expr ->Expr -> CodeWriter Operand
genBinOp outname op left right = do
    tmpid <- reserveTempIndex 1
    let leftname = Name $ printf "left%d" tmpid
    let rightname = Name $ printf "right%d" tmpid
    leftref <- genExpr leftname left
    rightref <- genExpr rightname right
    addInstr $ outname := op leftref rightref []
    return $ LocalReference double outname

genExpr :: Name -> Expr -> CodeWriter Operand
genExpr _ (EConstant v) = return $ ConstantOperand $ Float $ Double v
genExpr outname (EVariable var) = do
    t <- asks $ M.lookup var
    case t of
        Nothing -> fail $ "unknown variable " ++ var
        Just Param -> return $ LocalReference double (Name var)
        Just AllocA -> do
            addInstr $ outname := Load False (LocalReference double $ Name var) Nothing 0 []
            return $ LocalReference double outname
genExpr outname (EBinOp (Builtin '+') left right) = genBinOp outname (FAdd NoFastMathFlags) left right
genExpr outname (EBinOp (Builtin '-') left right) = genBinOp outname (FSub NoFastMathFlags) left right
genExpr outname (EBinOp (Builtin '*') left right) = genBinOp outname (FMul NoFastMathFlags) left right
genExpr outname (EBinOp (Builtin '<') left right) = do
    tmpid <- reserveTempIndex 1
    let tmpname = Name $ printf "cmp%d" tmpid
    genBinOp tmpname (FCmp ULT) left right
    addInstr $ outname := UIToFP (LocalReference i1 tmpname) double []
    return $ LocalReference double outname
genExpr outname (EBinOp (UserDefined f) left right) = genExpr outname (ECall f [left, right])

genExpr _ (EBinOp op _ _) = fail $ "invalid binop " ++ show op

genExpr outname (EUnaryOp f arg) = genExpr outname (ECall f [arg])

genExpr outname (ECall func args) = do
    baseid <- reserveTempIndex $ fromIntegral (length args)
    params <- zipWithM mkParam [baseid..] args
    addInstr $ outname := Call False C [] (Right $ ConstantOperand $ GlobalReference double $ Name func) params [] []
    return $ LocalReference double outname
    where
    mkParam i expr = do
        let paramname = Name $ printf "param%d" i
        ref <- genExpr paramname expr
        return (ref, [])

genExpr outname (EIf cond trueCase falseCase) = do
    tmpid <- reserveTempIndex 1
    let condretname = Name $ printf "cond%d" tmpid
    let condvalname = Name $ printf "condval%d" tmpid
    let thenblock = Name $ printf "then%d" tmpid
    let thenvalname = Name $ printf "thenval%d" tmpid
    let elseblock = Name $ printf "else%d" tmpid
    let elsevalname = Name $ printf "elseval%d" tmpid
    let contblock = Name $ printf "ifend%d" tmpid

    condret <- genExpr condretname cond
    addInstr $ condvalname := FCmp ONE (ConstantOperand $ Float $ Double 0) condret []
    startNewBlock (Do $ CondBr (LocalReference double condvalname) thenblock elseblock []) thenblock

    thenret <- genExpr thenvalname trueCase
    thenfinblock <- startNewBlock (Do $ Br contblock []) elseblock

    elseret <- genExpr elsevalname falseCase
    elsefinblock <- startNewBlock (Do $ Br contblock []) contblock

    addInstr $ outname := Phi double [(thenret, thenfinblock), (elseret, elsefinblock)] []
    return $ LocalReference double outname

genExpr _ (ELoop var start cond step body) = do
    tmpid <- reserveTempIndex 1
    let initval = Name $ printf "loopinit%d" tmpid
    let testval = Name $ printf "testval%d" tmpid
    let stepval = Name $ printf "stepval%d" tmpid
    let loopval = Name $ printf "loopval%d" tmpid
    let condval = Name $ printf "loopcond%d" tmpid
    let bodyval = Name $ printf "body%d" tmpid

    let condblock = Name $ printf "loop%dcond" tmpid
    let loopblock = Name $ printf "loop%dbody" tmpid
    let endblock = Name $ printf "loop%dend" tmpid

    initret <- genExpr initval start
    addInstr $ Name var := Alloca double Nothing 0 []
    addInstr $ Do $ Store False (LocalReference double $ Name var) initret Nothing 0 []
    startNewBlock (Do $ Br condblock []) condblock

    local (M.insert var AllocA) $ do
        testret <- genExpr testval cond
        addInstr $ condval := FCmp ONE (ConstantOperand $ Float $ Double 0) testret []
        startNewBlock (Do $ CondBr (LocalReference double condval) loopblock endblock []) loopblock

        genExpr bodyval body
        stepret <- genExpr stepval step
        addInstr $ stepval := Load False (LocalReference double $ Name var) Nothing 0 []
        addInstr $ loopval := FAdd NoFastMathFlags (LocalReference double stepval) stepret []
        addInstr $ Do $ Store False (LocalReference double $ Name var) (LocalReference double loopval) Nothing 0 []
        startNewBlock (Do $ Br condblock []) endblock

    return $ ConstantOperand $ Float $ Double 0

genExpr outname (ELet var initval expr) = do
    tmpid <- reserveTempIndex 1
    let initname = Name $ printf "varinit%d" tmpid

    initret <- genExpr initname initval
    addInstr $ Name var := Alloca double Nothing 0 []
    addInstr $ Do $ Store False (LocalReference double $ Name var) initret Nothing 0 []
    local (M.insert var AllocA) $ genExpr outname expr

genExpr outname (EAssign var expr) = do
    val <- genExpr outname expr
    addInstr $ Do $ Store False (LocalReference double $ Name var) val Nothing 0 []
    return val
