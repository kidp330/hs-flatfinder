{-# LANGUAGE OverloadedStrings #-}

module UnitTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified PrepareUtils
import qualified Data.Text.IO as TS

tests :: TestTree
tests = testGroup "unit" [
    testGroup "camelCaseStripPrefix" [
        testCase "T1" $ assertEqual "" "coherent"             (PrepareUtils.camelCaseStripPrefix "internallyCoherent"),
        testCase "T2" $ assertEqual "" "ruedeSaint-Ghislaine" (PrepareUtils.camelCaseStripPrefix "RuedeSaint-Ghislaine"),
        testCase "T3" $ assertEqual "" "04.03.52"             (PrepareUtils.camelCaseStripPrefix "04.03.52"),
        testCase "T4" $ assertEqual "" ""                     (PrepareUtils.camelCaseStripPrefix "")
    ],
    testGroup "Custom type parsing" [
    ]
  ]
-- TODO quickcheck tests

-- someOfferCategory = OfferCategory { ocId = 1
--                                   , ocLabel = ""
--                                   , ocParentId = 1
--                                   , ocName = ""
--                                   , ocNormalizedName = ""
--                                   , ocPosition = 1
--                                   , ocViewType = ""
--                                   , ocIconName = ""
--                                   , ocLevel = 1
--                                   , ocDisplayOrder = 1
--                                   , ocChildren = []
--                                   , ocPath = ""
--                                   , ocType = ""
--                                   , ocIsAdding = True
--                                   , ocIsSearch = True
--                                   , ocIsOfferSeek = False
--                                   , ocPrivateBusiness = False
--                                   , ocPhotosMax = 1
--                                   , ocImg = Nothing
--                                   }