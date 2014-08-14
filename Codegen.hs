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

genStatement :: Statement -> Definition
genStatement (SFunc fname args body) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = Name (BS8.unpack fname)
    }
genStatement (SExtern fname args) = GlobalDefinition $ functionDefaults {
        returnType = double,
        name = Name (BS8.unpack fname)
    }
genStatement (STopLevelExpr expr) = GlobalDefinition functionDefaults {
        returnType = double,
        name = UnName 0
    }
