module Syntax where


type Var = String

data Exp =
  Boo Bool |
  And Exp Exp |
  Num Integer |
  Add Exp Exp |
  Sub Exp Exp |
  Sma Exp Exp |
  Let Var Exp Exp |
  Var Var |
  Ite Exp Exp Exp |
  App Var [Exp]
    deriving (Show, Eq)

data Def = Fun Var [(Var, Type)] Type Exp
    deriving (Show, Eq)

type Prg = [Def]


data Type = TypeInt | TypeBool | TypeFunc [Type] Type
  deriving (Show, Eq)
