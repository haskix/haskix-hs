{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}

module Haskix.Haskix.Expr where

import Haskix.Haskix.Exts (HsixPs, XRec)

type role Expr nominal

data Expr pass

instance Show (Expr HsixPs)

type LExpr pass = XRec pass (Expr pass)

type role MatchGroup nominal nominal

data MatchGroup pass body

instance Show body => Show (MatchGroup HsixPs body)

type role GRHSs nominal nominal

data GRHSs pass body

instance Show body => Show (GRHSs HsixPs body)