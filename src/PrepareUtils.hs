{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PrepareUtils
    ( OfferCategory
    , parseCategories
    , testLog
    ) where

import GHC.Generics
import Data.Text
import qualified Data.Text.Lazy
import Data.Text.Encoding
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.Aeson

data OfferCategory = OfferCategory { ocId :: Word
                                   , ocLabel :: Text
                                   , ocParentId :: Word
                                   , ocName :: Text
                                   , ocNormalizedName :: Text
                                   , ocPosition :: Word
                                   , ocViewType :: Text -- change to enum maybe
                                   , ocIconName :: Text -- enum
                                   , ocLevel :: Word
                                   , ocDisplayOrder :: Word
                                   , ocChildren :: [Word]
                                   , ocPath :: Text
                                   , ocType :: Text -- enum
                                   , ocIsAdding :: Bool
                                   , ocIsSearch :: Bool
                                   , ocIsOfferSeek :: Bool
                                   , ocPrivateBusiness :: Bool
                                   , ocPhotosMax :: Word
                                   , ocImg :: Maybe Text
                                   } deriving(Generic, Show)

parseCategories :: Text -> [OfferCategory]
parseCategories = undefined

testLog :: Text -> IO ()
testLog = Data.ByteString.putStrLn . (maybe Data.ByteString.empty id) . decode . Data.ByteString.Lazy.fromStrict . Data.Text.Encoding.encodeUtf8