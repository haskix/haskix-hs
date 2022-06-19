module Haskix.Types.Name where

import Haskix.Types.Module
import Haskix.Types.Name.Occurrence

data NameSort
  = External Module
  | Internal
  deriving (Eq, Show)

data Name = Name
  { nSort :: NameSort,
    nOcc :: OccName
  }
  deriving (Eq, Show)