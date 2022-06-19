module Haskix.Types.Name.Reader where

import Haskix.Types.Module
import Haskix.Types.Name
import Haskix.Types.Name.Occurrence

data RdrName
  = Unqual OccName
  | Qual ModulePath OccName
  | Orig ModulePath OccName
  | Exact Name
  deriving (Eq, Show)