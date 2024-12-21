module IntermediateRepresentation where

import Syntax (
  Type)

-- | Labels
type Label = String

-- | Variables
type Var = String

-- | A program is a list of functions.
type Program = [Function]

-- | A functions with a name, arguments and a body.
data Function = Function Var [(Var, Type)] Type [JumpTarget]
  deriving (Show, Eq)

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
  Sub Val Val |
  And Val Val |
  Sma Val Val |
  Cal Var [Val]
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

