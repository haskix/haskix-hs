module Haskix.Lexer.Layout (layout) where

import Haskix.Haskix.Token

type StateFun = [PreToken] -> [Token]

generate :: TokenKind -> Token
generate g =
  WithOffset
    { woVal = g,
      woOffset = Nothing
    }

deciding :: StateFun -> [PreToken] -> [Token]
deciding _ [] = error "layout: Unexpected EOF"
deciding f pt@(WithOffset {woVal = ptk, woOffset = off} : xs) =
  case ptk of
    PtkIndent ic -> generate TkLeftBrace : implicit ic f xs
    PtkEol _ -> generate TkLeftBrace : generate TkRightBrace : generate TkSemicolon : f xs
    PtkToken t ->
      WithOffset {woVal = t, woOffset = off} :
      if t == TkLeftBrace
        then explicit 1 f xs
        else deciding f xs
    PtkDedent ic -> generate TkLeftBrace : generate TkRightBrace : generate TkSemicolon : f pt

decidingIf :: StateFun -> [PreToken] -> [Token]
decidingIf _ [] = error "layout: Unexpected EOF"
decidingIf f pt@(WithOffset {woVal = ptk, woOffset = off} : xs) =
  case ptk of
    PtkIndent ic -> generate TkLeftBrace : implicit ic f xs
    PtkDedent ic -> generate TkLeftBrace : generate TkRightBrace : f pt
    PtkToken t ->
      WithOffset {woVal = t, woOffset = off} :
      case t of
        TkInlineComment _ _ -> decidingIf f xs
        TkLeftBrace -> explicit 1 f xs
        _ -> f xs
    PtkEol _ -> generate TkLeftBrace : generate TkRightBrace : f xs

isDeciding :: TokenKind -> Bool
isDeciding x = x == TkWith || x == TkWhere || x == TkOf || x == TkUsing || x == TkDo

implicit :: IndentChange -> StateFun -> [PreToken] -> [Token]
implicit _ _ [] = []
implicit ic f (WithOffset {woVal = (PtkEol n)} : xs) =
  if n == icTo ic
    then generate TkSemicolon : implicit ic f xs
    else implicit ic f xs
implicit ic f (WithOffset {woVal = (PtkIndent new)} : xs) = implicit ic f xs
implicit ic f pt@(WithOffset {woVal = PtkDedent new} : xs)
  | icTo new < icFrom ic = generate TkRightBrace : generate TkSemicolon : f pt
  | icTo new < icTo ic = generate TkRightBrace : generate TkSemicolon : f xs
  | otherwise = implicit ic f xs
implicit ic f (WithOffset {woVal = PtkToken t, woOffset = off} : xs) =
  WithOffset {woVal = t, woOffset = off} : case t of
    TkIf -> decidingIf (implicit ic f) xs
    t | isDeciding t -> deciding (implicit ic f) xs
    _ -> implicit ic f xs

explicit :: Int -> StateFun -> [PreToken] -> [Token]
explicit 0 f xs = f xs
explicit _ _ [] = error "layout: Unexpected EOF"
explicit n f ((WithOffset {woVal = ptk, woOffset = off}) : xs) =
  case ptk of
    PtkToken t ->
      WithOffset {woVal = t, woOffset = off} :
      case t of
        TkIf -> decidingIf (explicit n f) xs
        TkLeftBrace -> explicit (n + 1) f xs
        TkRightBrace -> explicit (n - 1) f xs
        t | isDeciding t -> deciding (explicit n f) xs
        t -> explicit n f xs
    _ -> explicit n f xs

layout :: [PreToken] -> [Token]
layout = implicit (IndentChange 0 0) (const [])