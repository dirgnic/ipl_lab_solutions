module Lowering where

import Syntax (
  Exp)
import qualified Syntax as Syntax (
  Exp(..))

import IntermediateRepresentation (
  ANF(Block), Stm(Let), App, Val)
import qualified IntermediateRepresentation as IR (
  Var, App(..), Val(..))

import Control.Monad.Trans.State (
  State, get, put, evalState)

-- | Lower an expression from the source language to the intermediate
-- representation.
lower :: Exp -> ANF
lower expression = evalState (lowerExp expression) 0

-- | Lower an expression from the source language to the intermediate
-- representation using a state of int for fresh name generation.
lowerExp :: Exp -> State Int ANF
lowerExp (Syntax.Var x) = do
  return (Block [] (IR.Var x))
lowerExp (Syntax.IntLit n) = do
  return (Block [] (IR.Num n))
lowerExp (Syntax.BoolLit b) = do
  return (Block [] (IR.Boo b))
lowerExp (Syntax.Num n) = do
  return (Block [] (IR.Num n))
lowerExp (Syntax.Add lhs rhs) = do
  Block lhsBindings lhsResult <- lowerExp lhs
  Block rhsBindings rhsResult <- lowerExp rhs
  x <- fresh
  let binding = Let x (IR.Add lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (Block bindings result)
lowerExp (Syntax.Sub lhs rhs) = do
  Block lhsBindings lhsResult <- lowerExp lhs
  Block rhsBindings rhsResult <- lowerExp rhs
  x <- fresh
  let binding = Let x (IR.Sub lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (Block bindings result)
lowerExp (Syntax.Boo b) = do
  return (Block [] (IR.Boo b))
lowerExp (Syntax.And lhs rhs) = do
  Block lhsBindings lhsResult <- lowerExp lhs
  Block rhsBindings rhsResult <- lowerExp rhs
  x <- fresh
  let binding = Let x (IR.And lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (Block bindings result)
lowerExp (Syntax.Sma lhs rhs) = do
  Block lhsBindings lhsResult <- lowerExp lhs
  Block rhsBindings rhsResult <- lowerExp rhs
  x <- fresh
  let binding = Let x (IR.Sma lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (Block bindings result)
lowerExp (Syntax.Not expr) = do
  Block exprBindings exprResult <- lowerExp expr
  x <- fresh
  let binding = Let x (IR.Not exprResult)
  let bindings = exprBindings ++ [binding]
  let result = IR.Var x
  return (Block bindings result)
lowerExp (Syntax.Eq lhs rhs) = do
  Block lhsBindings lhsResult <- lowerExp lhs
  Block rhsBindings rhsResult <- lowerExp rhs
  x <- fresh
  let binding = Let x (IR.Eq lhsResult rhsResult)
  let bindings = lhsBindings ++ rhsBindings ++ [binding]
  let result = IR.Var x
  return (Block bindings result)
lowerExp (Syntax.Let x rhs body) = do
  Block rhsBindings rhsResult <- lowerExp rhs
  Block bodyBindings bodyResult <- lowerExp body
  let bindings = rhsBindings ++ substitute x rhsResult bodyBindings
  let result = substituteVal x rhsResult bodyResult
  return (Block bindings result)

-- | Substitute the variable with the value in the given list
-- of bindings.
substitute :: IR.Var -> Val -> [Stm] -> [Stm]
substitute _ _ [] = []
substitute x t (Let y u : bindings) =
  Let y (substituteApp x t u) : if x == y then bindings else substitute x t bindings

-- | Substitute the variable with the trivial expression in the given application.
substituteApp :: IR.Var -> Val -> App -> App
substituteApp x t (IR.Add l r) =
  IR.Add (substituteVal x t l) (substituteVal x t r)
substituteApp x t (IR.And l r) =
  IR.And (substituteVal x t l) (substituteVal x t r)
substituteApp x t (IR.Sma l r) =
  IR.Sma (substituteVal x t l) (substituteVal x t r)
substituteApp x t (IR.Sub l r) =
  IR.Sub (substituteVal x t l) (substituteVal x t r)
substituteApp x t (IR.Not v) =
  IR.Not (substituteVal x t v)
substituteApp x t (IR.Eq l r) =
  IR.Eq (substituteVal x t l) (substituteVal x t r)

-- | Substitute the variable with the trivial expression in the given trivial
-- expression.
substituteVal :: IR.Var -> Val -> Val -> Val
substituteVal _ _ (IR.Num n) = IR.Num n
substituteVal _ _ (IR.Boo b) = IR.Boo b
substituteVal x t (IR.Var y) = if x == y then t else (IR.Var y)

-- | Generate a fresh variable.
fresh :: State Int IR.Var
fresh = do
  i <- get
  put (i + 1)
  return ("x" ++ show i)
