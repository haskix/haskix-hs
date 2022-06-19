module Haskix.Types.Module where

data Visibility
  = VisPublic
  | VisSuper
  | VisFlake
  | VisSelf
  | VisPath ModulePath
  deriving (Eq, Show)

data ModPathComp
  = MpSuper
  | MpSelf
  | MpName String
  deriving (Eq, Show)

data ModulePath = ModulePath
  { mpFlake :: Bool,
    mpPath :: [ModPathComp]
  }
  deriving (Eq, Show)

data Module = Module
  { flake :: String,
    path :: [String]
  }
  deriving (Eq, Show)