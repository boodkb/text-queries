{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec
import           Test.Hspec.Attoparsec

import           Query

main :: IO ()
main = hspec $ do
  describe "parseQuery" $ do
    it "single value" $ do
      parseQuery "\"Waterfall\"" `shouldParse` Value "Waterfall"

    it "single value in parens" $ do
      parseQuery "(\"Waterfall\")" `shouldParse` Value "Waterfall"

    it "single value in parens and spaces" $ do
      parseQuery "( \"Waterfall\"  )" `shouldParse` Value "Waterfall"

    it "complex value" $ do
      parseQuery " \"Action\" OR ( \"Waterfall\" AND \"radius\" )"
        `shouldParse` Or (Value "Action")
                         (And (Value "Waterfall") (Value "radius"))

    it "parens in values" $ do
      parseQuery " \"Action\" OR ( \"Wa)terfall\" AND \"r(ad)ius\" )"
        `shouldParse` Or (Value "Action")
                         (And (Value "Wa)terfall") (Value "r(ad)ius"))

