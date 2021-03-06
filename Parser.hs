module Parser (parseProgram) where

import AST
import Control.Applicative
import Data.Text (Text)
import Text.Parsec.Prim (Parsec, runParser, getState, modifyState)
import Text.Parsec.Error
import Text.Parsec.Text ()
import Text.Parsec.Pos
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM

type Ops = IM.IntMap [Operator CodeParser Expr]
data ParserState = ParserState Ops (CodeParser Expr)
type CodeParser = Parsec Text ParserState

identStyle :: IdentifierStyle CodeParser
identStyle = IdentifierStyle "identifier" letter alphaNum
    (HS.fromList ["def", "extern", "if", "then", "for", "in", "binary", "unary", "var"]) Identifier ReservedIdentifier

defaultOps :: Ops
defaultOps = IM.fromAscListWith (++) [
    (2, [Infix (symbolic '=' *> return mkAssign) AssocNone]),
    (10, [Infix (symbolic '<' *> return (EBinOp $ Builtin '<')) AssocNone]),
    (20, [Infix (symbolic '+' *> return (EBinOp $ Builtin '+')) AssocLeft]),
    (20, [Infix (symbolic '-' *> return (EBinOp $ Builtin '-')) AssocLeft]),
    (40, [Infix (symbolic '*' *> return (EBinOp $ Builtin '*')) AssocLeft])
  ]
    where
    mkAssign left right = case left of
        EVariable name -> EAssign name right
        _ -> error "Variable requred in the left part of assignment"

buildExprParser :: Ops -> CodeParser Expr
buildExprParser ops = buildExpressionParser (map snd $ IM.toDescList ops) parseTerm

parseNumber :: CodeParser Expr
parseNumber = do
    v <- integerOrDouble
    return $ EConstant $ case v of
                            Right d -> d
                            Left i -> fromIntegral i

parseVar :: CodeParser Expr
parseVar = do
    v <- ident identStyle
    return $ EVariable v

parseCall :: CodeParser Expr
parseCall = do
    v <- ident identStyle
    args <- parens $ commaSep parseExpr
    return $ ECall v args

parseIf :: CodeParser Expr
parseIf = do
    symbol "if"
    cond <- parseExpr
    symbol "then"
    trueCase <- parseExpr
    symbol "else"
    falseCase <- parseExpr
    return $ EIf cond trueCase falseCase

parseFor :: CodeParser Expr
parseFor = do
    symbol "for"
    varname <- ident identStyle
    symbolic '='
    initExpr <- parseExpr
    symbolic ','
    condExpr <- parseExpr
    stepExpr <- option (EConstant 1.0) $ symbolic ',' *> parseExpr
    symbol "in"
    bodyExpr <- parseExpr
    return $ ELoop varname initExpr condExpr stepExpr bodyExpr

parseLet :: CodeParser Expr
parseLet = do
    symbol "var"
    let getvar = do
        v <- ident identStyle
        initexpr <- option (EConstant 0) $ symbolic '=' *> parseExpr
        return (v, initexpr)
    vars <- commaSep getvar
    symbol "in"
    subexpr <- parseExpr
    return $ foldr (\(v, initexpr) e -> ELet v initexpr e) subexpr vars

parseTerm :: CodeParser Expr
parseTerm =
    try parseLet <|> try parseFor <|> try parseIf <|> parseNumber <|> try parseCall <|> parseVar <|> parens parseExpr

parseExpr :: CodeParser Expr
parseExpr = do
    ParserState _ parser <- getState
    parser

parseStatement :: CodeParser Statement
parseStatement =
    parseExtern <|> try parseBinaryOp <|> try parseUnaryOp <|> parseFunc <|> parseTopLevelExpr

parseTopLevelExpr :: CodeParser Statement
parseTopLevelExpr = do
    e <- parseExpr
    return (STopLevelExpr e)

parsePrototype :: CodeParser (String, [String])
parsePrototype = do
    name <- ident identStyle
    params <- parens $ many (ident identStyle)
    return (name, params)

parseExtern :: CodeParser Statement
parseExtern = do
    symbol "extern"
    (name, args) <- parsePrototype
    return $ SExtern name args

parseFunc :: CodeParser Statement
parseFunc = do
    symbol "def"
    (name, args) <- parsePrototype
    body <- parseExpr
    return $ SFunc name args body

parseUnaryOp :: CodeParser Statement
parseUnaryOp = do
    symbol "def"
    string "unary"
    whiteSpace
    op <- anyChar
    whiteSpace
    param <- parens $ ident identStyle
    body <- parseExpr
    let func = 'u':op:"unop"
    let opdata = Prefix (symbolic op *> return (EUnaryOp func))
    let addop v = case v of
                    Nothing -> Just [opdata]
                    Just ops -> Just $ opdata : ops
    modifyState $ \(ParserState ops _) -> let newops = IM.alter addop maxBound ops
                                              parser = buildExprParser newops
                                           in ParserState newops parser
    return $ SFunc func [param] body

parseBinaryOp :: CodeParser Statement
parseBinaryOp = do
    symbol "def"
    string "binary"
    whiteSpace
    op <- anyChar
    whiteSpace
    prio <- decimal
    whiteSpace
    params <- parens $ do
                        param1 <- ident identStyle
                        param2 <- ident identStyle
                        return [param1, param2]
    body <- parseExpr
    let func = 'u':op:"binop"
    let opdata = Infix (symbolic op *> return (EBinOp $ UserDefined func)) AssocNone
    let addop v = case v of
                    Nothing -> Just [opdata]
                    Just ops -> Just $ opdata : ops
    modifyState $ \(ParserState ops _) -> let newops = IM.alter addop (fromIntegral prio) ops
                                              parser = buildExprParser newops
                                           in ParserState newops parser
    return $ SFunc func params body

programParser :: CodeParser [Statement]
programParser = do
    stmts <- parseStatement `sepEndBy` semi
    many $ symbolic ';'
    eof
    return stmts

parseProgram :: SourceName -> Text -> Either ParseError [Statement]
parseProgram = runParser programParser (ParserState defaultOps (buildExprParser defaultOps))
