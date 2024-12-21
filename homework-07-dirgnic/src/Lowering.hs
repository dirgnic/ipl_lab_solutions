module Lowering where

import Syntax (
  Stm(..), Exp(..))

import IntermediateRepresentation (
  Program, JoinPoint(JoinPoint), Block(Block), Val)
import qualified IntermediateRepresentation as IR (
  Var, Lab, Stm(Let), App(..), Val(..), Jump(..))

import Control.Monad.Trans.Class (
  lift)
import Control.Monad.Trans.State (
  State, get, put, evalState)
import Control.Monad.Trans.Writer (
  WriterT, tell, runWriterT)



-- | Lower a program from the source language to the intermediate
-- representation.
lower :: Stm -> Program
lower statement = let
  (entryBlock, program) = runLowering (lowerStm statement "return")
  returnJoinPoint = JoinPoint "return" (Block [] (IR.End (IR.Num 0)))
  entryJoinPoint = JoinPoint "entrypoint" entryBlock
  in ([entryJoinPoint] ++ program ++ [returnJoinPoint])


-- | Lower a statement. Jump to the continuation when finished.
lowerStm :: Stm -> IR.Lab -> Lowering Block
lowerStm Ski continuation = do
  return (Block [] (IR.Jum continuation))
lowerStm (Res expression) _continuation = do
  (bindings, result) <- lowerExp expression
  return (Block bindings (IR.End result))
lowerStm (Dcl reference restStatement) continuation = do
  x <- freshVariable
  let binding = IR.Let x (IR.Alo reference)
  Block restBindings restJump <- lowerStm restStatement continuation
  return (Block (binding : restBindings) restJump)
lowerStm (Set reference expression restStatement) continuation = do
  x <- freshVariable
  (bindings, result) <- lowerExp expression
  let binding = IR.Let x (IR.Sto reference result)
  Block restBindings restJump <- lowerStm restStatement continuation
  return (Block (bindings ++ [binding] ++ restBindings) restJump)
lowerStm (Ite cond thenBranch elseBranch rest) continuation = do
  -- Generate fresh labels
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  restLabel <- freshLabel

  -- Lower the condition
  (condBindings, condResult) <- lowerExp cond

  -- Emit the "then" branch
  thenBlock <- lowerStm thenBranch restLabel
  emitJoinPoint (JoinPoint thenLabel thenBlock)

  -- Emit the "else" branch
  elseBlock <- lowerStm elseBranch restLabel
  emitJoinPoint (JoinPoint elseLabel elseBlock)

  -- Emit the continuation
  restBlock <- lowerStm rest continuation
  emitJoinPoint (JoinPoint restLabel restBlock)

  -- Create the entry block that computes the condition and branches
  let entryBlock = Block condBindings (IR.Bra condResult thenLabel elseLabel)
  return entryBlock


lowerStm (Whi condition bodyStatement restStatement) continuation = do
  conditionLabel <- freshLabel
  bodyLabel <- freshLabel
  restLabel <- freshLabel
  (conditionBindings, conditionResult) <- lowerExp condition
  bodyBlock <- lowerStm bodyStatement conditionLabel
  restBlock <- lowerStm restStatement continuation
  let conditionBlock = Block conditionBindings (IR.Bra conditionResult bodyLabel restLabel)
  emitJoinPoint (JoinPoint conditionLabel conditionBlock)
  emitJoinPoint (JoinPoint bodyLabel bodyBlock)
  emitJoinPoint (JoinPoint restLabel restBlock)
  return (Block [] (IR.Jum conditionLabel))
lowerStm (New reference restStatement) continuation = do
  x <- freshVariable
  let binding = IR.Let x (IR.Mal reference)
  Block restBindings restJump <- lowerStm restStatement continuation
  return (Block (binding : restBindings) restJump)


-- | Lower an expression from the source language to the intermediate
-- representation. using a state of int for fresh name generation.
lowerExp :: Exp -> Lowering ([IR.Stm], Val)
lowerExp (Num n) = do
  return ([], IR.Num n)
lowerExp (Boo b) = do
  return ([], IR.Boo b)
lowerExp (Get reference) = do
  x <- freshVariable
  let bindings = [IR.Let x (IR.Loa reference)]
  return (bindings, IR.Var x)
lowerExp (Sma lhs rhs) = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (IR.Sma lhsResult rhsResult)
      bindings = lhsBindings ++ rhsBindings ++ [binding]
      result = IR.Var x
  return (bindings, result)
lowerExp (Add lhs rhs) = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (IR.Add lhsResult rhsResult)
      bindings = lhsBindings ++ rhsBindings ++ [binding]
      result = IR.Var x
  return (bindings, result)
lowerExp (And lhs rhs) = do
  (lhsBindings, lhsResult) <- lowerExp lhs
  (rhsBindings, rhsResult) <- lowerExp rhs
  x <- freshVariable
  let binding = IR.Let x (IR.And lhsResult rhsResult)
      bindings = lhsBindings ++ rhsBindings ++ [binding]
      result = IR.Var x
  return (bindings, result)


type Lowering = WriterT [JoinPoint] (State Int)

runLowering :: Lowering a -> (a, [JoinPoint])
runLowering m = evalState (runWriterT m) 0

-- | Generate a fresh variable.
freshVariable :: Lowering IR.Var
freshVariable = do
  i <- lift get
  lift (put (i + 1))
  return ("x" ++ show i)

-- | Generate a fresh label.
freshLabel :: Lowering IR.Lab
freshLabel = do
  i <- lift get
  lift (put (i + 1))
  return ("l" ++ show i)

-- | Emit a join point
emitJoinPoint :: JoinPoint -> Lowering ()
emitJoinPoint joinPoint = do
  tell [joinPoint]

