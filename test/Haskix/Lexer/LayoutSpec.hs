{-# LANGUAGE QuasiQuotes #-}

module Haskix.Lexer.LayoutSpec (spec) where

import Data.String.Interpolate
import Haskix.Haskix.Token
import Haskix.Lexer (lexer)
import Test.Hspec

shouldLexToKind :: String -> [TokenKind] -> Expectation
shouldLexToKind str k = (woVal <$> lexer (Just "<test>") str) `shouldBe` k

spec =
  describe "layout" $ do
    context "implicit" $ do
      it "semicolon in last line" $ do
        [__i|
          class T where
            a : a \n
        |]
          `shouldLexToKind` [ TkClass,
                              TkUpperName "T",
                              TkWhere,
                              TkLeftBrace,
                              TkLowerName "a",
                              TkColon,
                              TkLowerName "a",
                              TkSemicolon,
                              TkRightBrace,
                              TkSemicolon
                            ]