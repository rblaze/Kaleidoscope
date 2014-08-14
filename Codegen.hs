module Codegen (genCode) where

import AST
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import qualified Data.ByteString.Char8 as BS8

genCode :: [Statement] -> Module
genCode stmts = defaultModule {
                    moduleDefinitions = map genStatement stmts
                  }

mkName :: BS8.ByteString -> Name
mkName = Name . BS8.unpack

genStatement :: Statement -> Definition
genStatement (SFunc fname args body) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = mkName fname,
        parameters = (map (\n -> Parameter double (mkName n) []) args, False)
    }
genStatement (SExtern fname args) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = mkName fname,
        parameters = (map (\n -> Parameter double (mkName n) []) args, False)
    }
genStatement (STopLevelExpr expr) = GlobalDefinition functionDefaults {
        returnType = double,
        name = UnName 0
    }
