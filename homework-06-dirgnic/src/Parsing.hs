

module Parsing where

import Syntax (
  Prg, Def(..), Exp(..), Type(..))


import Data.Void (Void)

import Text.Megaparsec (
  Parsec, runParser, errorBundlePretty, many, some, eof, (<|>), between, sepBy)
import Text.Megaparsec.Char (
  space1, letterChar, char, string, digitChar)
import qualified Text.Megaparsec.Char.Lexer as Lexer (
  symbol, lexeme, space, decimal)

-- Type aliases
type ParseError = String
type Parser = Parsec Void String

-- Lexer Helpers
sc :: Parser ()
sc = Lexer.space space1 mempty mempty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parse a program
parsePrg :: Parser Prg
parsePrg = many parseDef <* eof

-- Parse a function definition
parseDef :: Parser Def
parseDef = do
  _ <- symbol "def"
  name <- parseVar
  params <- parens (parseParams `sepBy` symbol ",")
  _ <- symbol ":"
  returnType <- parseType
  _ <- symbol "="
  body <- parseExp
  return $ Fun name params returnType body

-- Parse parameters
parseParams :: Parser (String, Type)
parseParams = do
  name <- parseVar
  _ <- symbol ":"
  typ <- parseType
  return (name, typ)

-- Parse types
parseType :: Parser Type
parseType =
      (symbol "Int" >> return TypeInt)
  <|> (symbol "Bool" >> return TypeBool)

-- Parse expressions
parseExp :: Parser Exp
parseExp =
      parseLet
  <|> parseIf
  <|> parseApp
  <|> parseBinary
  <|> parseNum
  <|> parseBool
  <|> parseVarExp

-- Parse variable expressions
parseVarExp :: Parser Exp
parseVarExp = Var <$> parseVar

-- Parse variable names
parseVar :: Parser String
parseVar = lexeme ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))

-- Parse numeric literals
parseNum :: Parser Exp
parseNum = Num <$> lexeme Lexer.decimal

-- Parse boolean literals
parseBool :: Parser Exp
parseBool =
      (symbol "true" >> return (Boo True))
  <|> (symbol "false" >> return (Boo False))

-- Parse let expressions
parseLet :: Parser Exp
parseLet = do
  _ <- symbol "let"
  var <- parseVar
  _ <- symbol "="
  value <- parseExp
  _ <- symbol "in"
  body <- parseExp
  return $ Let var value body

-- Parse if-then-else expressions
parseIf :: Parser Exp
parseIf = do
  _ <- symbol "if"
  cond <- parseExp
  _ <- symbol "then"
  thn <- parseExp
  _ <- symbol "else"
  els <- parseExp
  return $ Ite cond thn els

-- Parse function application
parseApp :: Parser Exp
parseApp = do
  func <- parseVar
  args <- parens (parseExp `sepBy` symbol ",")
  return $ App func args

-- Parse binary operations
parseBinary :: Parser Exp
parseBinary = parseBinaryOp "+" Add <|> parseBinaryOp "-" Sub <|> parseBinaryOp "&&" And <|> parseBinaryOp "<" Sma

parseBinaryOp :: String -> (Exp -> Exp -> Exp) -> Parser Exp
parseBinaryOp op constructor = do
  lhs <- parseExp
  _ <- symbol op
  rhs <- parseExp
  return $ constructor lhs rhs

-- Parse a program from a string
parse :: String -> Either ParseError Prg
parse input = case runParser parsePrg "input" input of
  Left err -> Left $ errorBundlePretty err
  Right prg -> Right prg
