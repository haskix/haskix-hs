{-# LANGUAGE TypeFamilyDependencies #-}

module Haskix.Haskix.Exts where

import Data.Kind (Type)
import Haskix.Types.Name.Reader

data NoExtField = NoExtField
  deriving (Show)

data Pass
  = Parsed
  | Renamed
  | TypeChecked

data HaskixPass (c :: Pass)

type HsixPs = HaskixPass 'Parsed

type HsixRn = HaskixPass 'Renamed

type HsixTc = HaskixPass 'TypeChecked

type family NoHsixTcPass (c :: Pass) :: Pass where
  NoHsixTcPass 'TypeChecked = Renamed
  NoHsixTcPass other = other

type family NoHsixTc (p :: Type)

type instance NoHsixTc (HaskixPass p) = HaskixPass (NoHsixTcPass p)

newtype XRec pass a = XRec a
  deriving (Eq)

unXRec :: XRec p a -> a
unXRec (XRec v) = v

instance Show a => Show (XRec pass a) where
  show (XRec a) = show a

type family IdHsixP (p :: Pass) where
  IdHsixP 'Parsed = RdrName

type family IdP p

type instance IdP (HaskixPass p) = IdHsixP p

type LIdP p = XRec p (IdP p)
