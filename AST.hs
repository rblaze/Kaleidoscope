module AST where

data Statement
    = SFunc String [String] Expr
    | SExtern String [String]
    | STopLevelExpr Expr
    deriving Show

data BuiltinOrUser
    = Builtin Char
    | UserDefined String
    deriving Show

data Expr
    = EConstant Double
    | EVariable String
    | EBinOp BuiltinOrUser Expr Expr
    | EUnaryOp String Expr
    | ECall String [Expr]
    | EIf Expr Expr Expr
    | ELoop String Expr Expr Expr Expr
    | ELet String Expr Expr
    | EAssign String Expr
    deriving Show
