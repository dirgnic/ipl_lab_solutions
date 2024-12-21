module Lowering where

import Syntax (
  Prg, Def(Fun), Exp(..))

import IntermediateRepresentation (
  Program, Function(Function),
  JumpTarget(JumpTarget), Block(Block))
import qualified IntermediateRepresentation as IR (
  Var, Label, Binding(Let), App(..), Val(..), Jump(..))

import Control.Monad.Trans.Class (
  lift)
import Control.Monad.Trans.State (
  StateT, get, put, evalStateT)
import Control.Monad.Trans.Writer (
  Writer, tell, runWriter)


-- | Lower an expression from the source language to the intermediate
-- representation.
lower :: Prg -> Program
lower definitions = map lowerDef definitions


-- | Lower a function definition into an intermediate representation function.
lowerDef :: Def -> Function
lowerDef (Fun name parameters returnType body) = let
  returnPoint = JumpTarget "return" ["result"] (Block [] (IR.End (IR.Var "result")))
  (entryBlock, jumpTargets) = runLowering (lowerExp body (Continuation "return"))
  entryPoint = JumpTarget "entrypoint" [] entryBlock
  in (Function name parameters returnType ([entryPoint] ++ jumpTargets ++ [returnPoint]))


-- | Lower an expression from the source language to the intermediate
-- representation using a state of int for fresh name generation.
lowerExp :: Exp -> Continuation -> Lowering Block
lowerExp (Var x) continuation = do
  return (Block [] (lowerJump continuation (IR.Var x)))
lowerExp (Num n) continuation = do
  return (Block [] (lowerJump continuation (IR.Num n)))
lowerExp (Boo b) continuation = do
  return (Block [] (lowerJump continuation (IR.Boo b)))
lowerExp (Add lhs rhs) continuation = do
  callLabel <- freshLabel
  (block, yx) <- lowerArguments [lhs, rhs] callLabel
  let y = yx !! 0
  let x = yx !! 1
  z <- freshVariable
  let callJumpTarget = JumpTarget callLabel [] (Block [
        IR.Let z (IR.Add (IR.Var x) (IR.Var y))]
        (lowerJump continuation (IR.Var z)))
  emitJumpTarget callJumpTarget
  return block
lowerExp (Sub lhs rhs) continuation = do
  callLabel <- freshLabel
  (block, yx) <- lowerArguments [lhs, rhs] callLabel
  let y = yx !! 0
  let x = yx !! 1
  z <- freshVariable
  let callJumpTarget = JumpTarget callLabel [] (Block [
        IR.Let z (IR.Sub (IR.Var x) (IR.Var y))]
        (lowerJump continuation (IR.Var z)))
  emitJumpTarget callJumpTarget
  return block
lowerExp (Sma lhs rhs) continuation = do
  callLabel <- freshLabel
  (block, yx) <- lowerArguments [lhs, rhs] callLabel
  let y = yx !! 0
  let x = yx !! 1
  z <- freshVariable
  let callJumpTarget = JumpTarget callLabel [] (Block [
        IR.Let z (IR.Sma (IR.Var x) (IR.Var y))]
        (lowerJump continuation (IR.Var z)))
  emitJumpTarget callJumpTarget
  return block
lowerExp (And lhs rhs) continuation = do
  callLabel <- freshLabel
  (block, yx) <- lowerArguments [lhs, rhs] callLabel
  let y = yx !! 0
  let x = yx !! 1
  z <- freshVariable
  let callJumpTarget = JumpTarget callLabel [] (Block [
        IR.Let z (IR.And (IR.Var x) (IR.Var y))]
        (lowerJump continuation (IR.Var z)))
  emitJumpTarget callJumpTarget
  return block
lowerExp (Let x rhs body) continuation = do
  bodyLabel <- freshLabel
  rhsBlock <- lowerExp rhs (Continuation bodyLabel)
  bodyBlock <- lowerExp body continuation
  let bodyJumpTarget = JumpTarget bodyLabel (x : []) bodyBlock
  emitJumpTarget bodyJumpTarget
  return rhsBlock
lowerExp (Ite condition thenBranch elseBranch) continuation = do
  thenBranchLabel <- freshLabel
  elseBranchLabel <- freshLabel
  conditionBlock <- lowerExp condition (BranchContinuation thenBranchLabel elseBranchLabel)
  thenBranchBlock <- lowerExp thenBranch continuation
  elseBranchBlock <- lowerExp elseBranch continuation
  let thenBranchJumpTarget = JumpTarget thenBranchLabel [] thenBranchBlock
  let elseBranchJumpTarget = JumpTarget elseBranchLabel [] elseBranchBlock
  emitJumpTarget thenBranchJumpTarget
  emitJumpTarget elseBranchJumpTarget
  return conditionBlock
lowerExp (App function arguments) continuation = do
  callLabel <- freshLabel
  (argumentBlock, xs) <- lowerArguments arguments callLabel
  r <- freshVariable
  let callJumpTarget = JumpTarget callLabel [] (Block [
        IR.Let r (IR.Cal function (map IR.Var xs))]
        (lowerJump continuation (IR.Var r)))
  emitJumpTarget callJumpTarget
  return argumentBlock


-- | Lower the given list of expressions to code that executes the expressions
-- in sequence and binds the results to the returned list of variables. The
-- last expression jumps to the given label. The returned block corresponds
-- to the first expression.
lowerArguments :: [Exp] -> IR.Label -> Lowering (Block, [IR.Var])
lowerArguments [] continuation = do
  let lastBlock = Block [] (IR.Jwa continuation [])
  return (lastBlock, [])
lowerArguments (argument : arguments) continuation = do
  x <- freshVariable
  k <- freshLabel
  (argumentsBlock, xs) <- lowerArguments arguments continuation
  let argumentsJumpTarget = JumpTarget k (x : []) argumentsBlock
  emitJumpTarget argumentsJumpTarget
  argumentBlock <- lowerExp argument (Continuation k)
  return (argumentBlock, xs ++ [x])


-- | Lower a continuation, a primary argument and the rest of the arguments into
-- a jump.
lowerJump :: Continuation -> IR.Val -> IR.Jump
lowerJump (Continuation label) x =
  IR.Jwa label [x]
lowerJump (BranchContinuation thenLabel elseLabel) x =
  IR.Bwa x thenLabel [] elseLabel []


-- | A continuation is a single label to jump to or a pair of labels to branch to.
data Continuation =
  Continuation IR.Label |
  BranchContinuation IR.Label IR.Label


type Lowering = StateT Int (Writer [JumpTarget])

runLowering :: Lowering a -> (a, [JumpTarget])
runLowering m = runWriter (evalStateT m 0)

-- | Generate a fresh variable.
freshVariable :: Lowering IR.Var
freshVariable = do
  i <- get
  put (i + 1)
  return ("x" ++ show i)

-- | Generate a fresh label.
freshLabel :: Lowering IR.Label
freshLabel = do
  i <- get
  put (i + 1)
  return ("l" ++ show i)

-- | Emit a jump target
emitJumpTarget :: JumpTarget -> Lowering ()
emitJumpTarget jumpTarget = do
  lift (tell [jumpTarget])


