module AST where

import Data.ByteString

data Statement
    = SFunc ByteString [ByteString] Expr
    | SExtern ByteString [ByteString]
    | STopLevelExpr Expr
    deriving Show

data Expr
    = EConstant Double
    | EVariable ByteString
    | EBinOp Char Expr Expr
    | ECall ByteString [Expr]
    deriving Show
