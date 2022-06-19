module Haskix.Types.Var where

import Haskix.Types.Name

data Specificity
  = SpecInferred
  | SpecSpecified
  deriving (Show)

data Var = Var
  deriving (Show)

type TyVar = Var