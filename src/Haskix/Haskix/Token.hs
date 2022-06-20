module Haskix.Haskix.Token where

import Text.Megaparsec.Pos (SourcePos)

data IndentChange = IndentChange
  { icFrom, icTo :: Int
  }
  deriving (Eq, Show, Ord)

data Position = Position
  { posOffset :: Int,
    posSource :: SourcePos
  }
  deriving (Eq, Show, Ord)

data WithOffset a = WithOffset
  { woVal :: a,
    woOffset :: Maybe (Position, Position)
  }
  deriving (Eq, Show, Ord)

type PreToken = WithOffset PreTokenKind

data PreTokenKind
  = PtkIndent IndentChange
  | PtkDedent IndentChange
  | PtkEol Int
  | PtkToken TokenKind
  deriving (Eq, Show, Ord)

type Token = WithOffset TokenKind

data IntBase
  = IbBin
  | IbOct
  | IbDec
  | IbHex
  deriving (Eq, Show, Ord)

data TokenKind
  = TkBlock
  | TkCase
  | TkClass
  | TkData
  | TkDo
  | TkDeriving
  | TkElse
  | TkFlake
  | TkForall
  | TkIf
  | TkIn
  | TkInfix
  | TkInstance
  | TkLet
  | TkModule
  | TkNewtype
  | TkOf
  | TkOpen
  | TkPrecedence
  | TkPrivate
  | TkRecord
  | TkSelf
  | TkSuper
  | TkType
  | TkUsing
  | TkWhere
  | TkWith
  | TkTightInfixAt
  | TkSuffixBang
  | TkPrefixMinus
  | TkPrefixProj
  | TkTightInfixProj
  | TkPrefixAt
  | TkLowerName String
  | TkUpperName String
  | TkVarSymbol String
  | TkConstructorSymbol String
  | TkChar Char
  | TkString String
  | TkInteger
      { tkIBase :: Maybe IntBase,
        tkIVal :: String
      }
  | TkRational
      { tkRBase :: String,
        tkRExp :: Maybe String
      }
  | TkBlockComment
      { tkBcPrefix :: Maybe String,
        tkBcContent :: String
      }
  | TkInlineComment
      { tkIcPrefix :: Maybe String,
        tkIcContent :: String
      }
  | -- | {-@ MOD
    TkGlobalAttr
  | -- | {-@
    TkOpenAttr
  | -- | @-}
    TkCloseAttr
  | -- | )
    TkLeftParen
  | -- | (
    TkRightParen
  | -- | {
    TkLeftBrace
  | -- | }
    TkRightBrace
  | -- | [
    TkLeftBracket
  | -- | ]
    TkRightBracket
  | -- | ->
    TkArrow
  | -- | =>
    TkFatArrow
  | -- | ,
    TkComma
  | -- | :
    TkColon
  | -- | ;
    TkSemicolon
  | -- | =
    TkEqual
  | -- | \
    TkBackslash
  | -- | <-
    TkLeftArrow
  | -- | |
    TkBar
  | -- | ..
    TkDotDot
  | -- | _
    TkUnderline
  | -- | *
    TkStar
  deriving (Eq, Show, Ord)