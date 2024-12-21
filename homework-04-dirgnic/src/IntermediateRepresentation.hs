module IntermediateRepresentation where

import Syntax (Stm)
-- | Labels
type Label = String

-- | Variables
type Var = String

-- | A program in our intermeditate representation.
type Program = [JumpTarget]

-- | The target of a jump with arguments. The variables are bound in the block.
data JumpTarget = JumpTarget Label [Var] Block
  deriving (Show, Eq)

-- | A block is a list of bindings and ends in a jump.
data Block =
  Block [Binding] Jump
   deriving (Show, Eq)

data Binding =
  Let Var App
    deriving (Show, Eq)

data App =
  Add Val Val |
  And Val Val |
  For Val Var Val Stm | -- Added For construct
  Brk Val |            -- Added Break construct
  Sma Val Val
    deriving (Show, Eq)

data Val =
  Num Integer |
  Boo Bool |
  Var Var
    deriving (Show, Eq)

-- | Every block ends with a jump.
data Jump =
  End Val |
  -- ^ end computation with result
  Jwa Label [Val] |
  -- ^ jump with arguments
  Bwa Val Label [Val] Label [Val]
  -- ^ branch with arguments
    deriving (Show, Eq)
