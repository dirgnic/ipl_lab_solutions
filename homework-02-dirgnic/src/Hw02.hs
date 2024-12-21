{-# LANGUAGE OverloadedStrings #-}
module Hw02 where

import qualified LLVM.AST.Constant as LLVM (Constant(Int))
import qualified LLVM.AST as LLVM (Operand(LocalReference, ConstantOperand), Instruction(Add, Mul), Name, mkName)
import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  functionDefaults, Named(Do, (:=)),
  Instruction, Terminator(Ret), Operand)
import LLVM.AST.Global (Global(name, returnType, basicBlocks))
import LLVM.AST.Type (i64)
import Data.String (fromString)
import Control.Monad.Trans.State (State, get, put, evalState)
import qualified Data.Map as Map

type Var = String

data Exp =
  Lit Integer |
  Add Exp Exp |
  Mul Exp Exp |
  Var Var |
  Let Var Exp Exp

type CodegenState = (Int, Map.Map Var Operand)

codegen :: Exp -> State CodegenState ([Named Instruction], Operand)
codegen (Lit n) = do
  -- Generate a constant operand for a literal value
  return ([], LLVM.ConstantOperand (LLVM.Int 64 n))

codegen (Add e1 e2) = do
  -- Generate code for the left and right expressions
  (instrs1, op1) <- codegen e1
  (instrs2, op2) <- codegen e2
  -- Generate a fresh variable name
  var <- fresh
  -- Generate the LLVM Add instruction
  let addInstr = var := LLVM.Add False False op1 op2 []
  -- Return the combined instructions and reference to the result
  return (instrs1 ++ instrs2 ++ [addInstr], LLVM.LocalReference i64 var)

codegen (Mul e1 e2) = do
  -- Generate code for the left and right expressions
  (instrs1, op1) <- codegen e1
  (instrs2, op2) <- codegen e2
  -- Generate a fresh variable name
  var <- fresh
  -- Generate the LLVM Mul instruction
  let mulInstr = var := LLVM.Mul False False op1 op2 []
  -- Return the combined instructions and reference to the result
  return (instrs1 ++ instrs2 ++ [mulInstr], LLVM.LocalReference i64 var)

codegen (Var x) = do
  -- Look up the variable in the environment
  (_, env) <- get
  case Map.lookup x env of
    Just operand -> return ([], operand)
    Nothing -> error ("Unbound variable: " ++ x)

codegen (Let var e1 e2) = do
  -- Generate code for the binding expression
  (instrs1, op1) <- codegen e1
  -- Update the environment with the new variable
  (counter, env) <- get
  let uniqueVar = LLVM.mkName (var ++ show counter)
      newEnv = Map.insert var op1 env
  put (counter + 1, newEnv)
  -- Generate code for the body expression
  (instrs2, op2) <- codegen e2
  -- Return the combined instructions and result
  return (instrs1 ++ instrs2, op2)

fresh :: State CodegenState LLVM.Name
fresh = do
  -- Generate a unique name for each new variable
  (i, env) <- get
  put (i + 1, env)
  return (LLVM.mkName ("x" ++ show i))

codegenBasicBlock :: Exp -> BasicBlock
codegenBasicBlock expression = do
  -- Generate LLVM instructions and the final operand
  let (instructions, operand) = evalState (codegen expression) (0, Map.empty)
  -- Create a basic block with the instructions and return the final result
  BasicBlock "myEntrypoint" instructions (Do (Ret (Just operand) []))

codegenDefinition :: Exp -> Definition
codegenDefinition expression = GlobalDefinition (functionDefaults {
  -- Define the main function
  name = "myMain",
  returnType = i64,
  basicBlocks = [codegenBasicBlock expression]})

codegenModule :: String -> Exp -> Module
codegenModule moduleName expression =
  -- Create an LLVM module with the given name and definition
  Module (fromString moduleName) (fromString (moduleName ++ ".ae")) Nothing Nothing [codegenDefinition expression]
