module Parser (parseProgram) where

import AST
import Control.Applicative
import Text.Parsec.Text
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS

chain :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chain expr op = do
    l <- expr
    rest l
    where
    rest l =
        do { f <- op; r <- expr; rest $ f l r } <|> return l

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle "identifier" letter alphaNum
    (HS.fromList ["def", "extern", "if", "then", "for", "in"]) Identifier ReservedIdentifier

parseNumber :: Parser Expr
parseNumber = do
    v <- integerOrDouble
    return $ EConstant $ case v of
                            Right d -> d
                            Left i -> fromIntegral i

parseVar :: Parser Expr
parseVar = do
    v <- ident identStyle
    return $ EVariable v

parseCall :: Parser Expr
parseCall = do
    v <- ident identStyle
    args <- parens $ commaSep parseExpr
    return $ ECall v args

parseIf :: Parser Expr
parseIf = do
    symbol "if"
    cond <- parseExpr
    symbol "then"
    trueCase <- parseExpr
    symbol "else"
    falseCase <- parseExpr
    return $ EIf cond trueCase falseCase

parseFor :: Parser Expr
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

parseTerm :: Parser Expr
parseTerm =
    (try parseFor) <|> (try parseIf) <|> parseNumber <|> (try parseCall) <|> parseVar <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = parseCompare
    where
    parseCompare = parseAddSub `chain` parseBinOp (== '<')
    parseAddSub = parseMul `chain` parseBinOp (\c -> c == '+' || c == '-')
    parseMul = parseTerm `chain` parseBinOp (== '*')

parseBinOp :: (Char -> Bool) -> Parser (Expr -> Expr -> Expr)
parseBinOp cond = do
    op <- satisfy cond
    whiteSpace
    return $ EBinOp op

parseStatement :: Parser Statement
parseStatement =
    parseExtern <|> parseFunc <|> parseTopLevelExpr

parseTopLevelExpr :: Parser Statement
parseTopLevelExpr = do
    e <- parseExpr
    return (STopLevelExpr e)

parsePrototype :: Parser (String, [String])
parsePrototype = do
    name <- ident identStyle
    params <- parens $ commaSep $ ident identStyle
    return (name, params)

parseExtern :: Parser Statement
parseExtern = do
    symbol "extern"
    (name, args) <- parsePrototype
    return $ SExtern name args

parseFunc :: Parser Statement
parseFunc = do
    symbol "def"
    (name, args) <- parsePrototype
    body <- parseExpr
    return $ SFunc name args body

parseProgram :: Parser [Statement]
parseProgram = do
    stmts <- parseStatement `sepEndBy` semi
    many $ symbolic ';'
    eof
    return stmts
