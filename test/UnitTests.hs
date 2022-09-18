{-# LANGUAGE OverloadedStrings #-}

module UnitTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified PrepareUtils

tests :: TestTree
tests = testGroup "unit" [
    testGroup "camelCaseStripPrefix" [
        testCase "T1" $ assertEqual "" "coherent"             (PrepareUtils.camelcaseStripPrefix "internallyCoherent"),
        testCase "T2" $ assertEqual "" "ruedeSaint-Ghislaine" (PrepareUtils.camelcaseStripPrefix "RuedeSaint-Ghislaine"),
        testCase "T3" $ assertEqual "" "04.03.52"             (PrepareUtils.camelcaseStripPrefix "04.03.52"),
        testCase "T4" $ assertEqual "" ""                     (PrepareUtils.camelcaseStripPrefix "")
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