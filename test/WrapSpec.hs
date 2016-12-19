{-# LANGUAGE OverloadedStrings #-}
module WrapSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Appoint.Wrap (wrapLine, wrapParagraph)
import qualified Data.Text as T

main :: IO ()
main = hspec spec

term :: T.Text
term = "This PR factors out the loclist logic to be more generic and support the\nquickfix list (as well as moving it to its own file)."

term2 :: T.Text
term2 = "this is a\nstring that breaks\nevery n chars"

spec :: Spec
spec = do
  describe "Wrap.wrapLine" $ do
    it "ok right" $ do
      wrapLine 72 term `shouldBe` ([term] :: [T.Text])

  describe "Wrap.combined" $ do
    it "wraps correctly" $ do
      wrapParagraph 12 term2 `shouldBe` ("this is a\nstring that \nbreaks\nevery n char\ns" :: T.Text)

    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    prop "is inverse to show" $
      \x -> (read . show) x == (x :: Int)
