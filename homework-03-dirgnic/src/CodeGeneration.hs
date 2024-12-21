{-# LANGUAGE OverloadedStrings #-}


module CodeGeneration where

import Prelude (($))

import Prelude (String, Bool(..), map, (++), Maybe(..))  -- Explicitly import required Prelude constructs
import IntermediateRepresentation (
  ANF(Block), Stm(Let), App(..), Val(..))

import qualified LLVM.AST.Constant as LLVM (
  Constant(Int))
import qualified LLVM.AST as LLVM (
  Instruction(Add, Sub, And, ICmp, Xor), Operand(LocalReference, ConstantOperand))


import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  Operand, Named(Do, (:=)), mkName,
  Instruction, Terminator(Ret))
import LLVM.AST.Global (Global(..), functionDefaults)
import LLVM.AST.IntegerPredicate (IntegerPredicate(..))
import LLVM.AST.Type (i64, i1)
import Data.String (fromString)

-- | Generate an LLVM module for the given IR expression with name "myModule".
codegenModule :: String -> ANF -> Module
codegenModule moduleName expression =
  Module (fromString moduleName) (fromString (moduleName ++ ".baelv")) Nothing Nothing [codegenDefinition expression]

-- | Generate an LLVM definition of a single function "myMain" for the given IR expression.
codegenDefinition :: ANF -> Definition
codegenDefinition expression = GlobalDefinition $ functionDefaults
  { name = mkName "myMain",
    returnType = i64,
    basicBlocks = [codegenBasicBlock expression]
  }

-- | An LLVM basic block from the given IR expression.

codegenBasicBlock :: ANF -> BasicBlock
codegenBasicBlock (Block bindings result) =
  BasicBlock "myEntrypoint" (codegenInstructions bindings) (Do (Ret (Just (codegenOperandAsInt result)) []))

-- | Generate a list of instructions from the given list of bindings.
codegenInstructions :: [Stm] -> [Named Instruction]
codegenInstructions bindings = map codegenInstruction bindings

-- | Generate a single instruction from the given binding.
codegenInstruction :: Stm -> Named Instruction
codegenInstruction (Let x (Add y z)) =
  mkName x := LLVM.Add False False (codegenOperandAsInt y) (codegenOperandAsInt z) []
codegenInstruction (Let x (Sub y z)) =
  mkName x := LLVM.Sub False False (codegenOperandAsInt y) (codegenOperandAsInt z) []
codegenInstruction (Let x (And y z)) = 
  mkName x := LLVM.And (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (Not y)) =
  mkName x := LLVM.Xor (codegenOperandAsBool y) (LLVM.ConstantOperand (LLVM.Int 1 1)) []
codegenInstruction (Let x (Eq y z)) =
  mkName x := LLVM.ICmp EQ (codegenOperandAsInt y) (codegenOperandAsInt z) []
codegenInstruction (Let x (Sma y z)) =
  mkName x := LLVM.ICmp SLT (codegenOperandAsInt y) (codegenOperandAsInt z) []

-- | Generate an LLVM operand from the given trivial expression.
codegenOperand :: Val -> Operand
codegenOperand (Num n) = LLVM.ConstantOperand (LLVM.Int 64 n)
codegenOperand (Boo False) = LLVM.ConstantOperand (LLVM.Int 1 0)
codegenOperand (Boo True) = LLVM.ConstantOperand (LLVM.Int 1 1)
codegenOperand (Var x) = LLVM.LocalReference i64 (mkName x)


codegenOperandAsInt :: Val -> LLVM.Operand
codegenOperandAsInt (Boo True)  = LLVM.ConstantOperand (LLVM.Int 64 1)
codegenOperandAsInt (Boo False) = LLVM.ConstantOperand (LLVM.Int 64 0)
codegenOperandAsInt (Num n)     = LLVM.ConstantOperand (LLVM.Int 64 n)
codegenOperandAsInt (Var x)     = LLVM.LocalReference i64 (mkName x)
codegenOperandAsInt val = case codegenOperand val of
  LLVM.LocalReference _ name ->
    let extendedName = mkName "extended_tmp"
    in LLVM.LocalReference i64 extendedName
  op -> op


codegenOperandAsBool :: Val -> LLVM.Operand
codegenOperandAsBool (Boo True)  = LLVM.ConstantOperand (LLVM.Int 1 1)
codegenOperandAsBool (Boo False) = LLVM.ConstantOperand (LLVM.Int 1 0)
codegenOperandAsBool (Var x)     = LLVM.LocalReference (LLVM.AST.Type.i1) (mkName x)
