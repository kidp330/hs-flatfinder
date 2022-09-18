{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

module PrepareUtils where

import GHC.Generics
import qualified Data.Text as TS
import qualified Data.ByteString.Lazy as BL
import Data.List (singleton)
import Data.Char
import Data.Aeson as Aeson
import Data.Map (Map)
import Data.Foldable (fold)
import qualified Data.Map as Map

-- TODO refactor file structure and terrible naming schemes

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
instance Semigroup FilterOption where
    (<>) fo1 fo2 = FilterOption { foCategories = (foCategories fo1) <> (foCategories fo2) 
                                , foRanges = mempty
                                , foConstraints = mempty
                                }
instance Monoid FilterOption where
    mempty = FilterOption mempty mempty mempty

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

data FilterInfo = FilterInfo { fiParamName :: FilterParamName
                             , fiLabel :: TS.Text
                             , fiCategories :: [CategoryId]
                             } deriving (Show)
data FilterTypeInfo = EnumFilter     { efValues :: [FilterEnumValue] 
                                     }
                    | CheckboxFilter
                    | ValueFilter    { vfUnit :: Maybe TS.Text
                                     , vfRanges :: [Int]
                                     , vfConstraints :: Maybe TS.Text -- TODO for textfield validation
                                     } deriving (Show)
type Filter = (FilterInfo, FilterTypeInfo)

separateRecordTree :: (Foldable f, Functor f, forall a. Monoid (f a)) => (r -> f r0) -> (r -> r0 -> r') -> f r -> f r'
separateRecordTree extractFieldList singleValueConstructor = fold . fmap (
        \r -> fmap (\r0 -> singleValueConstructor r r0) $ extractFieldList r
    )

camelCaseStripPrefix :: String -> String
camelCaseStripPrefix label = uncapitalize stripped
    where stripped = dropWhile isLower label
          uncapitalize "" = ""
          uncapitalize (x:xs) = (toLower x : xs)

convertToReducedFilter :: FilterParamName -> CategoryFilter -> FilterOption -> Filter
convertToReducedFilter fpn cf fo = ( FilterInfo fpn (cfLabel cf) (foCategories fo)
                                   , case cfType cf of 
                                        "checkbox"   -> CheckboxFilter
                                        "checkboxes" -> EnumFilter (cfValues cf)
                                        "select"     -> EnumFilter (cfValues cf)
                                        "price"      -> makeValueFilter cf fo
                                        "value"      -> makeValueFilter cf fo
                                        _            -> error $ "Found filter of unknown type: " ++ (TS.unpack fpn)
                                   )
    where makeValueFilter cf' fo' = ValueFilter (cfUnit cf') (foRanges fo') (Map.lookup "type" $ foConstraints fo')

type FilterStruct = [Filter]
type CategoryStruct = Map CategoryId OfferCategory

parseFilterJson :: BL.ByteString -> [(FilterParamName, [CategoryFilter])]
parseFilterJson jsonBStr = case Aeson.eitherDecode jsonBStr of
    Left err -> error $ "Failed to parse filters with the following message: " ++ err
    Right m  -> Map.toList m

flattenParsedFilters :: [(FilterParamName, [CategoryFilter])] -> [(FilterParamName, CategoryFilter)]
flattenParsedFilters = separateRecordTree snd (\(fpn, _) cf -> (fpn, cf))

convertCategoryFilter :: [(FilterParamName, CategoryFilter)] -> [Filter]
convertCategoryFilter xs = let
    nonValueFiltersJoinedOptions = map (\(fpn, cf) -> (fpn, cf { cfOptions = singleton $ mconcat $ cfOptions cf })) xs
    valueFiltersFlattenedOptions = separateRecordTree (cfOptions . snd) (uncurry convertToReducedFilter) nonValueFiltersJoinedOptions
    in valueFiltersFlattenedOptions

createFilters :: BL.ByteString -> FilterStruct
createFilters = convertCategoryFilter . flattenParsedFilters . parseFilterJson

getCategoryFilters :: FilterStruct -> CategoryId -> [Filter]
getCategoryFilters fs cid = filter (\(fi, _) -> cid `elem` (fiCategories fi)) fs

parseCategoriesJson :: BL.ByteString -> Map String OfferCategory
parseCategoriesJson jsonBStr = case Aeson.eitherDecode jsonBStr of
    Left err -> error $ "Failed to parse categories with the following message: " ++ err
    Right m  -> m

createCategories :: BL.ByteString -> CategoryStruct 
createCategories = Map.mapKeys (read :: String -> Word) . parseCategoriesJson

getCategory :: CategoryStruct -> CategoryId -> Maybe OfferCategory
getCategory = flip Map.lookup

-- TODO how to make sure these structures are not recomputed?