{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hw02 (
  Exp(..), codegenModule)

import LLVM.AST (Module)

import LLVM.Module (
  withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (
  withContext)
import LLVM.Target (
  withHostTargetMachineDefault)


myExpression :: Exp
myExpression = Let "x" (Add (Lit 5) (Lit 10)) (Add (Var "x") (Var "x"))


-- | An LLVM module with name \"myModule\" and a single definition.
myModule :: Module
myModule = codegenModule "myModule" myExpression


-- | Write out an object file with the given name corresponding to the given
-- LLVM module.
compileModule :: FilePath -> Module -> IO ()
compileModule targetFile modul = do
  withContext (\context ->
    withModuleFromAST context modul (\modulePtr ->
      withHostTargetMachineDefault (\targetMachine ->
        writeObjectToFile targetMachine (File targetFile) modulePtr)))


-- | Write out an object file \"myModule.o\" corresponding to the constant
-- LLVM module 'myModule'.
main :: IO ()
main = compileModule "myModule.o" myModule


