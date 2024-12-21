module Syntax where


type Var = String

data Exp =
  Boo Bool |
  And Exp Exp |
  Num Integer |
  Add Exp Exp |
  Sma Exp Exp |
  Var Var
    deriving (Show, Eq)

data Stm =
  Res Exp |
  Let Var Stm Stm |
  Ite Exp Stm Stm |
  For Exp Var Exp Stm
   deriving (Show, Eq)

