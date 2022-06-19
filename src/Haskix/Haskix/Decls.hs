{-# LANGUAGE FlexibleInstances #-}

module Haskix.Haskix.Decls where

import Data.Void
import Haskix.Haskix.Attrib
import Haskix.Haskix.Binds
import Haskix.Haskix.Exts
import Haskix.Haskix.Type
import Haskix.Types.Fixity (Assoc)
import Haskix.Types.Module
import Haskix.Types.Var

newtype DerivingClause pass = DerivingClause
  { derivClauseTys :: [LType pass]
  }

deriving instance Show (DerivingClause HsixPs)

type LDerivingClause pass = XRec pass (DerivingClause pass)

type Deriving pass = [LDerivingClause pass]

type ConDeclADTDetails pass = ConDetails Void (LType pass) (XRec pass [LConDeclField pass])

data ConDecl pass = ConDeclADT
  { conAttrib :: LAttrib pass,
    conVis :: Maybe Visibility,
    conName :: LIdP pass,
    conForall :: Bool,
    conExTv :: [LTyVarBndr Specificity pass],
    conArgs :: ConDeclADTDetails pass
  }

deriving instance Show (ConDecl HsixPs)

type LConDecl pass = XRec pass (ConDecl pass)

data DataCon pass
  = DcConDecl [LConDecl pass]
  | DcRecDecl [LConDeclField pass]

deriving instance Show (DataCon HsixPs)

data DataDefn pass = DataDefn
  { ddKindSig :: Maybe (LKind pass),
    ddCon :: DataCon pass,
    ddDerivs :: [Deriving pass]
  }

deriving instance Show (DataDefn HsixPs)

data TyClDecl pass
  = SynDecl
      { tcdAttrib :: LAttrib pass,
        tcdLName :: LIdP pass,
        tcdTyVars :: LQTyVars pass,
        tcdRHS :: LType pass
      }
  | DataDecl
      { tcdAttrib :: LAttrib pass,
        tcdLName :: LIdP pass,
        tcdTyVars :: LQTyVars pass,
        tcdDataDefn :: DataDefn pass
      }
  | ClassDecl
      { tcdCtxt :: Maybe [LContext pass],
        tcdLName :: LIdP pass,
        tcdTyVars :: LQTyVars pass,
        tcdSigs :: [LSig pass],
        tcdMeths :: LBinds pass
      }

deriving instance Show (TyClDecl HsixPs)

data ClsInstDecl pass = ClsInstDecl
  { cidPolyTy :: [LSigType pass],
    cidBinds :: LBinds pass,
    cidSigs :: [LSig pass]
  }

deriving instance Show (ClsInstDecl HsixPs)

newtype InstDecl pass = ClsIndtD
  { cidInst :: ClsInstDecl pass
  }

deriving instance Show (InstDecl HsixPs)

data PrecGrpDecl pass = PrecGrpDecl
  { pgdName :: LIdP pass,
    pgdAbove :: [LIdP pass],
    pgdBelow :: [LIdP pass],
    pgdAssoc :: Maybe Assoc
  }

deriving instance Show (PrecGrpDecl HsixPs)

data ModBody pass = ModBody
  { mdbAttr :: LAttribs pass,
    mdbBody :: [LDecl pass]
  }

deriving instance Show (ModBody HsixPs)

data ModDecl pass = ModDecl
  { modName :: String,
    modAttrib :: LAttribs pass,
    modBody :: Maybe (ModBody pass)
  }

deriving instance Show (ModDecl HsixPs)

data OpenTree pass
  = OtSuper [OpenTree pass]
  | OtSelf [OpenTree pass]
  | OtName String [OpenTree pass]

deriving instance Show (OpenTree HsixPs)

data OpenTreeRoot pass
  = OtrFlake [OpenTree pass]
  | OtrOther (OpenTree pass)

deriving instance Show (OpenTreeRoot HsixPs)

data OpenDecl pass = OpenDecl
  { odAttr :: LAttribs pass,
    odTree :: OpenTreeRoot pass
  }

deriving instance Show (OpenDecl HsixPs)

data Decl pass
  = TyClD (Maybe Visibility) (TyClDecl pass)
  | InstD (InstDecl pass)
  | ModD (Maybe Visibility) (ModDecl pass)
  | OpenD (Maybe Visibility) (OpenDecl pass)
  | ValD (Bind pass)
  | PrecGrpD (Maybe Visibility) (PrecGrpDecl pass)
  | BlockD (LAttribs pass) (Maybe Visibility) [Decl pass]

deriving instance Show (Decl HsixPs)

type LDecl pass = XRec pass (Decl pass)