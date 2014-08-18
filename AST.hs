module AST where

data Statement
    = SFunc String [String] Expr
    | SExtern String [String]
    | STopLevelExpr Expr
    deriving Show

data Expr
    = EConstant Double
    | EVariable String
    | EBinOp Char Expr Expr
    | ECall String [Expr]
    | EIf Expr Expr Expr
    | ELoop String Expr Expr Expr Expr
    deriving Show
