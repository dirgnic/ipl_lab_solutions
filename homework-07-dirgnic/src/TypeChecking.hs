module TypeChecking where

import Syntax (
  Stm(..), Exp(..), Ref)

import Control.Monad.Trans.Class (
  lift)
import Control.Monad.Trans.Reader (
  ReaderT, ask, runReaderT, local)
import Control.Monad.Trans.Except (
  Except, throwE, runExcept)


data Type = TypeInt | TypeBool
  deriving (Show, Eq)

data TypeError = TypeMismatch | NotInScope
  deriving (Show, Eq)

type Environment = [(Ref, Type)]

-- | The empty environment.
emptyEnvironment :: Environment
emptyEnvironment = []


-- | Type check the given program. A result of 'Nothing' means everything
-- is ok.
typeCheck :: Stm -> Maybe TypeError
typeCheck statement =
  case runExcept (runReaderT (checkStm statement) emptyEnvironment) of
    Left typeError -> Just typeError
    Right _ -> Nothing


-- | Type check the given statement, throwing a type error.

checkStm :: Stm -> TypeChecking ()
checkStm Ski = return ()
checkStm (Res expr) = do
  _ <- inferExp expr
  return ()
checkStm (Dcl ref stm) = 
  withLocalReferenceType ref TypeInt (checkStm stm)
checkStm (Set ref expr stm) = do
  refType <- lookupReference ref
  exprType <- inferExp expr
  assertTypeEqual refType exprType
  checkStm stm
checkStm (Ite cond thenBranch elseBranch rest) = do
  condType <- inferExp cond
  assertTypeEqual condType TypeBool
  checkStm thenBranch
  checkStm elseBranch
  checkStm rest
checkStm (Whi cond body rest) = do
  condType <- inferExp cond
  assertTypeEqual condType TypeBool
  checkStm body
  checkStm rest
checkStm (New ref stm) = 
  withLocalReferenceType ref TypeInt (checkStm stm)



-- | Type check the given expression, either returning its type or throwing a
-- type error.
inferExp :: Exp -> TypeChecking Type
inferExp (Boo _) = do
  return TypeBool
inferExp (And lhs rhs) = do
  lhsType <- inferExp lhs
  rhsType <- inferExp rhs
  assertTypeEqual lhsType TypeBool
  assertTypeEqual rhsType TypeBool
  return TypeBool
inferExp (Num _) = do
  return TypeInt
inferExp (Add lhs rhs) = do
  lhsType <- inferExp lhs
  rhsType <- inferExp rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeInt
inferExp (Sma lhs rhs) = do
  lhsType <- inferExp lhs
  rhsType <- inferExp rhs
  assertTypeEqual lhsType TypeInt
  assertTypeEqual rhsType TypeInt
  return TypeBool
inferExp (Get x) = do
  typ <- lookupReference x
  return typ


type TypeChecking a = ReaderT Environment (Except TypeError) a

assertTypeEqual :: Type -> Type -> TypeChecking ()
assertTypeEqual typ1 typ2 = do
  if typ1 == typ2
    then do
      return ()
    else do
      throwTypeError TypeMismatch

throwTypeError :: TypeError -> TypeChecking a
throwTypeError e = lift (throwE e)

lookupReference :: Ref -> TypeChecking Type
lookupReference x = do
  environment <- ask
  case lookup x environment of
    Nothing -> do
      throwTypeError NotInScope
    Just typ -> do
      return typ

withLocalReferenceType :: Ref -> Type -> TypeChecking a -> TypeChecking a
withLocalReferenceType x t m = local (\e -> (x, t) : e) m

