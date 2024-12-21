module Parsing where

import Syntax(
  Stm(..), Exp(..))

import Text.Megaparsec (
  runParser, Parsec, errorBundlePretty,
  (<|>), many, between, eof)
import Text.Megaparsec.Char (
  space, space1, char, letterChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer (
  decimal)
import qualified Text.Megaparsec.Char.Lexer as Lexer (
  symbol)
import Control.Monad.Combinators.Expr (
  makeExprParser, Operator(InfixL))

import Data.Void (
  Void)


type ParseError = String



-- | Parse the given string into a statement.
parse :: String -> Either ParseError Stm
parse file = case runParser parseStm "myModule" file of
  Left parseError -> Left (errorBundlePretty parseError)
  Right statement -> Right statement


-- | The type of parser, specialized over string.
type Parser = Parsec Void String


-- | Parse exactly the given string and consume any whitspace after it.
symbol :: String -> Parser ()
symbol s = do
  _ <- Lexer.symbol space1 s
  return ()

separator :: Char -> Parser ()
separator c = do
  _ <- char c
  space
  return ()

semicolon :: Parser ()
semicolon = separator ';'

identifier :: Parser String
identifier = do
  c <- letterChar
  cs <- many alphaNumChar
  space
  return (c : cs)


-- | Parse a statement which is a sequence of assignments terminated by a return.
parseStm :: Parser Stm
parseStm =
  parseSkip <|>
  parseReturn <|>
  parseDeclaration <|>
  parseNew <|>
  parseIfThenElse <|>
  parseWhile <|>
  parseAssignment <|>
  parseEnd

parseSkip :: Parser Stm
parseSkip = do
  symbol "skip"
  return Ski

parseReturn :: Parser Stm
parseReturn = do
  symbol "return"
  expression <- parseExp
  return (Res expression)

parseDeclaration :: Parser Stm
parseDeclaration = do
  symbol "int"
  reference <- identifier
  semicolon
  restStatement <- parseStm
  return (Dcl reference restStatement)

parseNew :: Parser Stm
parseNew = do
  symbol "new int"
  reference <- identifier
  semicolon
  restStatement <- parseStm
  return (New reference restStatement)

parseAssignment :: Parser Stm
parseAssignment = do
  reference <- identifier
  symbol "="
  expression <- parseExp
  semicolon
  restStatement <- parseStm
  return (Set reference expression restStatement)

parseIfThenElse :: Parser Stm
parseIfThenElse = do
  symbol "if"
  condition <- between (separator '(') (separator ')') parseExp
  thenBranch <- between (separator '{') (separator '}') parseStm
  symbol "else"
  elseBranch <- between (separator '{') (separator '}') parseStm
  semicolon
  restStatement <- parseStm
  return (Ite condition thenBranch elseBranch restStatement)

parseWhile :: Parser Stm
parseWhile = do
  symbol "while"
  condition <- between (separator '(') (separator ')') parseExp
  bodyStatement <- between (separator '{') (separator '}') parseStm
  semicolon
  restStatement <- parseStm
  return (Whi condition bodyStatement restStatement)

parseEnd :: Parser Stm
parseEnd = do
  eof
  return Ski


-- | Parse an expression which is an integer literal, a variable or and expression
-- added to another expression.
parseExp :: Parser Exp
parseExp = makeExprParser literalParser [
  [InfixL (symbol "+" >> return Add)],
  [InfixL (symbol "<" >> return Sma)],
  [InfixL (symbol "&" >> return And)]]


literalParser :: Parser Exp
literalParser =
  parseBool <|>
  parseNumber <|>
  parseDereference

parseBool :: Parser Exp
parseBool = let
  parseTrue = do
    symbol "true"
    return (Boo True)
  parseFalse = do
    symbol "false"
    return (Boo False)
  in parseTrue <|> parseFalse

parseNumber :: Parser Exp
parseNumber = do
  number <- decimal
  space
  return (Num number)

parseDereference :: Parser Exp
parseDereference = do
  reference <- identifier
  return (Get reference)

