module Parsing (parseStmWrap, parse_func) where

import Syntax (Stm(..), Exp(..), Var)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void

-- Parser type
type Parser = Parsec Void String

-- Top-level wrapper: parse a complete _func

parseStmWrap :: Parser Stm
parseStmWrap = do
  optional eof
  spaceConsumer
  stmts <- many parseStatement            -- Parse all valid statements
  optional eof
  symbol "return"
  expr <- parseExp                        -- Parse the return elgjlkkgkljdklmlgksjnrlksg/mklgnetklshjejiovjyopression
  symbol ";"                              -- Ensure a trailing semicolon
  spaceConsumer                     -- Consume trailing whitespace or newlines
  eof
  if null stmts
    then return (Res expr)                -- If no statements, just return
    else return (Seq stmts (Res expr))    -- Combine parsed statements

-- Exported parse function to run the parser
parse_func :: String -> Either (ParseErrorBundle String Void) Stm
parse_func input = runParser parseStmWrap "" input

-- Parse a generic statement
parseStatement :: Parser Stm
parseStatement = try parseLet <|> parseIfElse

-- Parse a "let" statement with type inference
parseLet :: Parser Stm
parseLet = do
  optional eof
  symbol "let"
  var <- parseVar >>= notReserved        -- Ensure variable name is valid
  symbol "="
  expr <- parseExp
  symbol ";"                           -- Ensure a semicolon
  spaceConsumer                         -- Consume trailing whitespace or newlines
  return $ Let var expr (Res (Num 0))

-- Parse an "if-else" statement


-- Parse an "if-else" statement
parseIfElse :: Parser Stm
parseIfElse = do
  optional eof
  symbol "if"
  cond <- parens parseExp                 -- Parse condition in parentheses
  thenBranch <- braces parseIfElseWrap       -- Parse "then" block
  optional eof
  symbol "else"
  elseBranch <- braces parseIfElseWrap       -- Parse "else" block
  optional eof
  return (Ite cond thenBranch elseBranch)

parseIfElseWrap :: Parser Stm
parseIfElseWrap = do
  spaceConsumer
  stmts <- many parseStatement            -- Parse all valid statements
  symbol "return"
  expr <- parseExp                        -- Parse the return elgjlkkgkljdklmlgksjnrlksg/mklgnetklshjejiovjyopression
  symbol ";"                              -- Ensure a trailing semicolon
  spaceConsumer                     -- Consume trailing whitespace or newlines
  
  if null stmts
    then return (Res expr)                -- If no statements, just return
    else return (Seq stmts (Res expr))  

-- Check that a variable is not a reserved keyword
notReserved :: String -> Parser String
notReserved var =
  if var `elem` ["if", "else", "return", "let", "int", "bool", "true", "false"]
    then fail ("Unexpected reserved keyword: " ++ var)
    else return var

-- Parse expressions with operator precedence
parseExp :: Parser Exp
parseExp = parseLogicalAndExp

-- Parse logical AND expressions
parseLogicalAndExp :: Parser Exp
parseLogicalAndExp = do
  lhs <- parseEqualityExp
  rest lhs
  where
    rest lhs = (do
      symbol "&&"
      rhs <- parseEqualityExp
      rest (And lhs rhs)) <|> return lhs

-- Parse equality expressions
parseEqualityExp :: Parser Exp
parseEqualityExp = do
  lhs <- parseComparisonExp
  rest lhs
  where
    rest lhs = (do
      symbol "=="
      rhs <- parseComparisonExp
      rest (Eq lhs rhs)) <|> return lhs

-- Parse comparison expressions (e.g., <)
parseComparisonExp :: Parser Exp
parseComparisonExp = do
  lhs <- parseAddExp
  rest lhs
  where
    rest lhs = (do
      symbol "<"
      rhs <- parseAddExp
      return (Sma lhs rhs)) <|> return lhs

-- Parse addition expressions
parseAddExp :: Parser Exp
parseAddExp = do
  lhs <- parseTerm
  rest lhs
  where
    rest lhs = (do
      symbol "+"
      rhs <- parseTerm
      rest (Add lhs rhs)) <|> return lhs

-- Parse terms: numbers, booleans, variables, or parenthesized expressions
parseTerm :: Parser Exp
parseTerm =
      (Num <$> Lexer.decimal)          -- Parse numeric literals
  <|> (Boo True <$ symbol "true")      -- Parse "true"
  <|> (Boo False <$ symbol "false")    -- Parse "false"
  <|> (Var <$> parseVar)               -- Parse variables
  <|> parens parseExp                  -- Parenthesized expressions

-- Parse a variable: a sequence of letters
parseVar :: Parser Var
parseVar = some letterChar <* spaceConsumer

-- Helpers for whitespace, symbols, parentheses, and braces
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

symbol :: String -> Parser String
symbol = Lexer.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
