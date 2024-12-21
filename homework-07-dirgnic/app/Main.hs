{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parsing (
  parse)
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
import System.Exit (
  exitFailure)


-- | Compile the file with the name given on the command line and write out
-- an object file with name \"myModule.o\".
main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [filepath] -> do
      file <- readFile filepath
      case parse file of
        Left parseError -> do
          putStrLn parseError
          exitFailure
        Right statement -> do
          case typeCheck statement of
            Just typeError -> do
              putStrLn (show typeError)
              exitFailure
            Nothing -> do
              let myModule = codegen (lower statement)
              withContext (\context ->
                withModuleFromAST context myModule (\modulePtr ->
                  withHostTargetMachineDefault (\targetMachine ->
                    writeObjectToFile targetMachine (File "myModule.o") modulePtr)))
    _ -> do
      putStrLn "Commandline Error: Please pass a single file name on the command line."
      exitFailure


