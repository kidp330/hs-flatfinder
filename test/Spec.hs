{-# LANGUAGE OverloadedStrings #-}

import PrepareUtils
import Data.Text

main :: IO ()
main = do
  json <- readFile "test/resources/categories_test.json"
  testLog $ pack json
