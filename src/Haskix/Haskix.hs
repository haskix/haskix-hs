{-# LANGUAGE FlexibleInstances #-}

module Haskix.Haskix where

import Haskix.Haskix.Decls (ModBody)
import Haskix.Haskix.Exts (HsixPs)

data FileMod pass = FileMod
  { fmBody :: ModBody pass,
    fmSubmodule :: [String]
  }

deriving instance Show (FileMod HsixPs)