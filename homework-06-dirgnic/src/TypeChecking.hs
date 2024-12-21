module TypeChecking where

import Syntax (
  Type(..), Prg, Def(..), Exp(..), Var)

import Control.Monad.Trans.Except (
  Except, throwE, runExcept)


data TypeError = TypeMismatch | NotInScope
  deriving (Show, Eq)

type Environment = [(Var, Type)]

-- | The empty environment.
emptyEnvironment :: Environment
emptyEnvironment = []


-- | Type check the given program. A result of 'Nothing' means everything
-- is ok.
typeCheck :: Prg -> Maybe TypeError
typeCheck program = case runExcept (typeCheckPrg program) of
  Left typeError -> Just typeError
  Right _ -> Nothing

typeCheckPrg :: Prg -> TypeChecking ()
typeCheckPrg defs = do
  let env = [(name, typ) | Fun name _ typ _ <- defs]
  mapM_ (typeCheckFunction env) defs


typeCheckFunction :: Environment -> Def -> TypeChecking ()
typeCheckFunction env (Fun name parameters annotatedType body) = do
  let paramEnv = map (\(var, typ) -> (var, typ)) parameters
  let extendedEnv = paramEnv ++ env
  inferredType <- inferExp extendedEnv body
  assertTypeEqual annotatedType inferredType

-- | Type check the given expression, either returning its type or throwing a
-- type error.
inferExp :: Environment -> Exp -> TypeChecking Type
inferExp vs (Boo _) = do
  return TypeBool
inferExp vs (And lhs rhs) = do
  lhsType <- inferExp vs lhs
  rhsType <- inferExp vs rhs
  assertTypeEqual lhsType TypeBool
  assertTypeEqual rhsType TypeBool
  return TypeBool
inferExp vs (Num _) = do
  return TypeInt
inferExp vs (Add lhs rhs) = do
  lhsType <- inferExp vs lhs
  rhsType <- inferExp vs rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeInt
inferExp vs (Sub lhs rhs) = do
  lhsType <- inferExp vs lhs
  rhsType <- inferExp vs rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeInt
inferExp vs (Sma lhs rhs) = do
  lhsType <- inferExp vs lhs
  rhsType <- inferExp vs rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeBool
inferExp vs (Var x) = do
  case lookup x vs of
    Nothing -> do
      throwTypeError NotInScope
    Just typ -> do
      return typ
inferExp vs (Let x body rest) = do
  t <- inferExp vs body
  inferExp ((x, t) : vs) rest
inferExp vs (Ite cond thn els) = do
  t <- inferExp vs cond
  assertTypeEqual t TypeBool
  t1 <- inferExp vs thn
  t2 <- inferExp vs els
  assertTypeEqual t1 t2
  return t1
inferExp vs (App f args) = do
  case lookup f vs of
    Nothing -> throwTypeError NotInScope
    Just (TypeFunc paramTypes returnType) -> do
      argTypes <- mapM (inferExp vs) args
      if paramTypes == argTypes
        then return returnType
        else throwTypeError TypeMismatch
    Just _ -> throwTypeError TypeMismatch -- Handle case where the type of `f` is not a function




type TypeChecking a = Except TypeError a

assertTypeEqual :: Type -> Type -> TypeChecking ()
assertTypeEqual typ1 typ2 = do
  if typ1 == typ2
    then do
      return ()
    else do
      throwTypeError TypeMismatch

throwTypeError :: TypeError -> TypeChecking a
throwTypeError e = throwE e

