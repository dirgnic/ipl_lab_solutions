{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parsing (
  parse_func)
import TypeChecking (
  typeCheck)
import Lowering (
  lower)
import CodeGeneration (
  codegen)

import LLVM.Module (
  withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (
  withContext)
import LLVM.Target (
  withHostTargetMachineDefault)

import System.Environment (
  getArgs)

import Text.Megaparsec (errorBundlePretty)

-- | Compile the file with the name given on the command line and write out
-- an object file with name \"myModule.o\".
main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [filepath] -> do
      file <- readFile filepath
      case parse_func file of
        Left parseError -> do
          putStrLn (errorBundlePretty parseError)
        Right statement -> do
          case typeCheck statement of
            Left typeError -> putStrLn (show typeError)
            Right _        -> do
                -- Lowering and code generation logic goes here
              let loweredProgram = lower statement
              let myModule = codegen loweredProgram
              withContext $ \context ->
                withModuleFromAST context myModule $ \modulePtr ->
                  withHostTargetMachineDefault $ \targetMachine ->
                    writeObjectToFile targetMachine (File "myModule.o") modulePtr


