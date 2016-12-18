{-# LANGUAGE OverloadedStrings #-}
module Appoint.Wrap where
import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T

reverseBreak :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
reverseBreak f xs = (T.reverse before, T.reverse after)
  where (after, before) = T.break f $ T.reverse xs

wrapLine :: Int -> T.Text -> [T.Text]
wrapLine maxLen line 
  | T.length line <= maxLen = [line]
  | T.any isSpace beforeMax = beforeSpace : wrapLine maxLen (afterSpace <> afterMax)
  | otherwise = beforeMax : wrapLine maxLen afterMax
    where (beforeMax, afterMax) = T.splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax

wrapParagraph :: Int -> T.Text -> T.Text
wrapParagraph maxLen text = foldl (\a b -> a <> lineEnd b) "" combinedLines
  where
    lines' = T.lines text
    wrappedLines = fmap (wrapLine maxLen) lines'
    combinedLines = fmap (T.intercalate "\n") wrappedLines
    lineEnd l
      | T.null l = "\n"
      | T.last l == '\n' = l
      | otherwise = l <> "\n"
