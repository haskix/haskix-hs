module Haskix.Lexer (lexer) where

import Haskix.Haskix.Token
import Haskix.Lexer.Layout (layout)
import qualified Haskix.Lexer.Lex as Lex (lex)

lexer :: Maybe String -> String -> [Token]
lexer file = layout . Lex.lex file