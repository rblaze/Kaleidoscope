module Parser (parseProgram) where

import AST
import Control.Applicative
import Data.Text
import Text.Parsec.Prim (Parsec, runParser, getState)
import Text.Parsec.Error
import Text.Parsec.Text ()
import Text.Parsec.Pos
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS

data ParserState = ParserState (OperatorTable CodeParser Expr)
type CodeParser = Parsec Text ParserState

identStyle :: IdentifierStyle CodeParser
identStyle = IdentifierStyle "identifier" letter alphaNum
    (HS.fromList ["def", "extern", "if", "then", "for", "in"]) Identifier ReservedIdentifier

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

parseTerm :: CodeParser Expr
parseTerm =
    try parseFor <|> try parseIf <|> parseNumber <|> try parseCall <|> parseVar <|> parens parseExpr

parseExpr :: CodeParser Expr
parseExpr = do
    ParserState ops <- getState
    buildExpressionParser ops parseTerm

parseStatement :: CodeParser Statement
parseStatement =
    parseExtern <|> parseFunc <|> parseTopLevelExpr

parseTopLevelExpr :: CodeParser Statement
parseTopLevelExpr = do
    e <- parseExpr
    return (STopLevelExpr e)

parsePrototype :: CodeParser (String, [String])
parsePrototype = do
    name <- ident identStyle
    params <- parens $ commaSep $ ident identStyle
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

programParser :: CodeParser [Statement]
programParser = do
    stmts <- parseStatement `sepEndBy` semi
    many $ symbolic ';'
    eof
    return stmts

defaultOps :: OperatorTable CodeParser Expr
defaultOps = [
    [ Infix (symbolic '*' *> return (EBinOp '*')) AssocLeft],
    [ Infix (symbolic '+' *> return (EBinOp '+')) AssocLeft,
            Infix (symbolic '-' *> return (EBinOp '-')) AssocLeft],
    [ Infix (symbolic '<' *> return (EBinOp '<')) AssocNone]
  ]

parseProgram :: SourceName -> Text -> Either ParseError [Statement]
parseProgram = runParser programParser (ParserState defaultOps)
