module WordNumberSpec where

import WordNumber (digitToWord, digits, wordNumber)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ do digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do digitToWord 1 `shouldBe` "one"
