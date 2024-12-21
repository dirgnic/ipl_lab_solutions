module Syntax where


type Ref = String

data Exp =
  Boo Bool |
  And Exp Exp |
  Num Integer |
  Add Exp Exp |
  Sma Exp Exp |
  Get Ref
    deriving (Show, Eq)

data Stm =
  Ski |
  Res Exp |
  Dcl Ref Stm |
  Set Ref Exp Stm |
  Ite Exp Stm Stm Stm |
  Whi Exp Stm Stm |
  New Ref Stm
   deriving (Show, Eq)

