{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module PrepareUtils where

import GHC.Generics
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Aeson as Aeson
import Data.Map (Map)
import Data.Foldable (fold)
import qualified Data.Map as Map

type CategoryId = Word

data OfferCategory = OfferCategory { ocId :: CategoryId
                                   , ocParentId :: CategoryId
                                   , ocNormalizedName :: TS.Text
                                   , ocLevel :: Word
                                   , ocChildren :: [CategoryId]
                                   , ocPath :: TS.Text
                                   } deriving (Generic, Show)
instance FromJSON OfferCategory where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier =  camelCaseStripPrefix })

data FilterOption = FilterOption { foCategories :: [CategoryId]
                                 , foRanges :: [Int]
                                 , foConstraints :: Map TS.Text TS.Text -- {"type":["string"|"integer"|"float"]}
                                 } deriving (Generic, Show)
instance FromJSON FilterOption where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier =  camelCaseStripPrefix })

data FilterEnumValue = FilterEnumValue { fevLabel :: TS.Text
                                       , fevValue :: TS.Text
                                       } deriving (Generic, Show)
instance FromJSON FilterEnumValue where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier =  camelCaseStripPrefix })

type FilterParamName = TS.Text

data CategoryFilter = CategoryFilter { cfType :: TS.Text -- enum
                                     , cfLabel :: TS.Text
                                     , cfUnit :: Maybe TS.Text
                                     , cfValues :: [FilterEnumValue]
                                     , cfOptions :: [FilterOption]
                                     } deriving (Generic, Show)
instance FromJSON CategoryFilter where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier =  camelCaseStripPrefix })


camelCaseStripPrefix :: String -> String
camelCaseStripPrefix label = uncapitalize stripped
    where stripped = dropWhile isLower label
          uncapitalize "" = ""
          uncapitalize (x:xs) = (toLower x : xs)

stretchAssociative :: (Foldable f, Functor f, forall a. Monoid (f a)) => f (k, f v) -> f (k, v)
stretchAssociative = fold . fmap stretch 
    where stretch (key, vs) = fmap (\val -> (key, val)) vs

categoryFilters :: BL.ByteString -> [(FilterParamName, CategoryFilter)]
categoryFilters jsonBStr = 
    case Aeson.eitherDecode jsonBStr of
        Left err -> error $ "Failed to parse filters with the following message: " ++ err
        Right m  -> stretchAssociative $ Map.toList m

categoryMap :: BL.ByteString -> (Word -> Maybe OfferCategory) 
categoryMap jsonBStr = 
    case Aeson.eitherDecode jsonBStr of
        Left err -> error $ "Failed to parse categories with the following message: " ++ err
        Right m  -> 
            let keyConvertedMap = Map.mapKeys (read :: String -> Word) m
            in \x -> Map.lookup x keyConvertedMap