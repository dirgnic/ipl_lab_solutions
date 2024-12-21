{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration where

import Syntax (
  Type(TypeInt, TypeBool))
import IntermediateRepresentation (
  Program, Function(Function), JumpTarget(JumpTarget), Var, Label,
  Block(Block), Binding(Let), App(..), Val(..),
  Jump(..))

import qualified LLVM.AST.Constant as LLVM (
  Constant(Int, GlobalReference))
import qualified LLVM.AST as LLVM (
  Type(FunctionType), Instruction(Add, Sub, And, ICmp, Call, Phi))

import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  Operand(LocalReference, ConstantOperand),
  functionDefaults, Named(Do, (:=)), mkName,
  Instruction, Terminator(Ret, Br, CondBr), Operand)
import LLVM.AST.Global (
  Global(name, returnType, parameters, basicBlocks), Parameter(Parameter))
import LLVM.AST.CallingConvention (
  CallingConvention(C))
import LLVM.AST.IntegerPredicate (
 IntegerPredicate(ULT))
import LLVM.AST.Type (
  i1, i64)

import Data.List (
  transpose)
import Control.Monad (
  guard)
import Data.String (
  fromString)


-- | Generate an LLVM module for the given program with name \"myModule\".
codegen :: Program -> Module
codegen program = codegenModule "myModule" program


-- | An LLVM module with the given name for the given program.
-- It consists of a single function definition.
codegenModule :: String -> Program -> Module
codegenModule moduleName program =
  Module (fromString moduleName) (fromString (moduleName ++ ".program")) Nothing Nothing (map codegenDefinition program)


-- | An LLVM definition of a single function.
codegenDefinition :: Function -> Definition
codegenDefinition (Function functionName parameterList typ body) = GlobalDefinition (
  functionDefaults {
    name = mkName functionName,
    returnType = i64,
    parameters = (map (\(parameterName, parameterType) ->
      Parameter i64 (mkName parameterName) []) parameterList, False),
    basicBlocks = codegenBasicBlocks body})


-- | An LLVM basic block from the given jump target.
codegenBasicBlocks :: [JumpTarget] -> [BasicBlock]
codegenBasicBlocks program =
  map (\jumpTarget -> codegenBasicBlock (findPredecessors program jumpTarget) jumpTarget) program


type Predecessors = [(Label, [Val])]

-- | An LLVM basic block from the given jump target.
codegenBasicBlock :: Predecessors -> JumpTarget -> BasicBlock
codegenBasicBlock predecessors (JumpTarget label parameterList (Block bindings jump)) = do
  let phiInstructions = codegenPhiInstructions parameterList predecessors
  let blockInstructions = codegenInstructions bindings
  let instructions = phiInstructions ++ blockInstructions
  let terminator = codegenTerminator jump
  BasicBlock (mkName label) instructions terminator


-- | Generate the phi instructions at the beginning of a basic block.
codegenPhiInstructions :: [Var] -> Predecessors -> [Named Instruction]
codegenPhiInstructions variables predecessors =
  zipWith codegenPhiInstruction variables (transposePredecessors predecessors)

-- | Generate a single phi instructions that binds the given variable.
codegenPhiInstruction :: Var -> [(Label, Val)] -> Named Instruction
codegenPhiInstruction variable predecessors = do
  let incoming = map (\(label, value) -> (codegenOperand value, mkName label)) predecessors
  mkName variable := LLVM.Phi i64 incoming []


-- | Given an association list of predecessor label to list of arguments, returns
-- a list of lists of pairs of label and argument.
-- All predecessors have to be associated to the same number of arguments.
-- The outer list has as many elements as there are arguments.
-- The inner lists have as many elements as there are predecessors.
transposePredecessors :: Predecessors -> [[(Label, Val)]]
transposePredecessors predecessors =
  transpose (map (\(label, arguments) -> map (\argument -> (label, argument)) arguments) predecessors)


-- | An LLVM terminator that jumps to the next basic block or ends the program.
codegenTerminator :: Jump -> Named Terminator
codegenTerminator (End result) =
  Do (Ret (Just (codegenOperand result)) [])
codegenTerminator (Jwa label _) =
  Do (Br (mkName label) [])
codegenTerminator (Bwa condition label1 _ label2 _) =
  Do (CondBr (codegenOperand condition) (mkName label1) (mkName label2) [])


-- | Generate an LLVM operand from the given trivial expression.
codegenOperand :: Val -> Operand
codegenOperand (Num n) = ConstantOperand (LLVM.Int 64 n)
codegenOperand (Boo False) = ConstantOperand (LLVM.Int 1 0)
codegenOperand (Boo True) = ConstantOperand (LLVM.Int 1 1)
codegenOperand (Var x) = LocalReference i64 (mkName x)

-- | Translate our types to LLVM types
mkType :: Type -> LLVM.Type
mkType TypeInt = i64
mkType TypeBool = i1


-- | Generate a list of instructions from the given list of bindings.
codegenInstructions :: [Binding] -> [Named Instruction]
codegenInstructions bindings = map codegenInstruction bindings


-- | Generate a single instruction from the given binding.
codegenInstruction :: Binding -> Named Instruction
codegenInstruction (Let x (Add y z)) =
  mkName x := LLVM.Add False False (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (Sub y z)) =
  mkName x := LLVM.Sub False False (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (And y z)) =
  mkName x := LLVM.And (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (Sma y z)) =
  mkName x := LLVM.ICmp ULT (codegenOperand y) (codegenOperand z) []
codegenInstruction (Let x (Cal functionName trivialArguments)) = let
  function = Right (ConstantOperand (LLVM.GlobalReference (mkName functionName)))
  arguments = map (\argument -> (codegenOperand argument, [])) trivialArguments
  functionType = LLVM.FunctionType i64 (map (\_ -> i64) trivialArguments) False
  in mkName x := LLVM.Call Nothing C [] functionType function arguments [] []


-- | Find for the given jump target the list of predecessors.
findPredecessors :: [JumpTarget] -> JumpTarget -> Predecessors
findPredecessors program (JumpTarget targetLabel _ _) = do
  JumpTarget predecessorLabel _ (Block _ jump) <- program
  case jump of
    End _ ->
      []
    Jwa jumpLabel arguments -> do
      guard (jumpLabel == targetLabel)
      [(predecessorLabel, arguments)]
    Bwa _ jumpLabel1 arguments1 jumpLabel2 arguments2 -> do
      let predecessor1 = do
            guard (jumpLabel1 == targetLabel)
            return (predecessorLabel, arguments1)
      let predecessor2 = do
            guard (jumpLabel2 == targetLabel)
            return (predecessorLabel, arguments2)
      predecessor1 ++ predecessor2

