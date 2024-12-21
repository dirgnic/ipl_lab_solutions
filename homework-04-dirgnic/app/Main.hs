{-# LANGUAGE OverloadedStrings #-}
module Main where

import Syntax (
  Stm(..), Exp(..))
import TypeChecking (
  typeCheck, TypeError)
import Lowering (
  lower)
import CodeGeneration (
  codegen)

import LLVM.AST (
  Module)
import LLVM.Module (
  withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (
  withContext)
import LLVM.Target (
  withHostTargetMachineDefault)


-- | A constant example statement used for compilation.
myStatement :: Stm
myStatement =
  Ite (Sma (Num 8) (Num 9))
    (Res (Num 1))
    (Res (Num 2))

-- | The compiler pipeline that first type checks the given expression and upon
-- success lowers it and generates code.
compile :: Stm -> Either TypeError Module
compile statement = case typeCheck statement of
  Just typeError -> Left typeError
  Nothing -> Right (codegen (lower statement))

-- | Write out an object file \"myModule.o\" corresponding to the constant
-- expression 'myExpression' or print a type error if the expression does
-- not type check.
main :: IO ()
main = do
  case compile myStatement of
    Left typeError -> do
      print typeError
    Right myModule -> do
      withContext (\context ->
        withModuleFromAST context myModule (\modulePtr ->
          withHostTargetMachineDefault (\targetMachine ->
            writeObjectToFile targetMachine (File "myModule.o") modulePtr)))


