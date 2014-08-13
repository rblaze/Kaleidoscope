module Parser where

import AST
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

chain :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chain expr op = do
    l <- expr
    rest l
    where
    rest l =
        do { f <- op; r <- expr; rest $ f l r } <|> return l

parseNumber :: Parser Expr
parseNumber = do
    skipSpace
    v <- double
    return $ EConstant v

parseParen :: Parser Expr
parseParen = do
    skipSpace
    char '('
    v <- parseExpr
    skipSpace
    char ')'
    return v

parseId :: Parser ByteString
parseId = do
    skipSpace
    takeWhile1 (\c -> isAlpha_ascii c || isDigit c)

parseVar :: Parser Expr
parseVar = do
    v <- parseId
    return $ EVariable v

parseCall :: Parser Expr
parseCall = do
    v <- parseId
    skipSpace
    char '('
    args <- (parseExpr <* skipSpace) `sepBy` char ','
    char ')'
    return $ ECall v args

parseTerm :: Parser Expr
parseTerm = do
    skipSpace
    parseNumber <|> parseParen <|> parseCall <|> parseVar

parseExpr :: Parser Expr
parseExpr = parseCompare
    where
    parseCompare = parseAddSub `chain` parseBinOp (== '<')
    parseAddSub = parseMul `chain` parseBinOp (\c -> c == '+' || c == '-')
    parseMul = parseTerm `chain` parseBinOp (== '*')

parseBinOp :: (Char -> Bool) -> Parser (Expr -> Expr -> Expr)
parseBinOp cond = do
    skipSpace
    op <- satisfy cond
    return $ EBinOp op
