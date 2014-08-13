module AST where

import Data.ByteString

data Statement
    = SProto ByteString [ByteString]
    | SFunc ByteString [ByteString] Expr
    deriving Show

data Expr
    = EConstant Double
    | EVariable ByteString
    | EBinOp Char Expr Expr
    | ECall ByteString [Expr]
    deriving Show
