-- {-# LANGUAGE OverloadedStrings #-}

-- import PrepareUtils

-- main :: IO ()
-- main = do
--   json <- readFile "test/resources/categories_test.json"
--   testLog $ pack json
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (defaultMain, testGroup)
import qualified UnitTests (tests)

main :: IO ()
main = defaultMain (testGroup "Tests" [UnitTests.tests])