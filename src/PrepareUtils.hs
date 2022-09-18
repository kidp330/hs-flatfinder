{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Text.Casing as Casing
import qualified Data.Map as Map

-- TODO refactor file structure and terrible naming schemes

-- stand in type before integration testing
type JsonString = BL.ByteString

type CategoryId = Word

data OfferCategory = OfferCategory { ocId :: CategoryId
                                   , ocParentId :: CategoryId
                                   , ocNormalizedName :: TS.Text
                                   , ocLevel :: Word
                                   , ocChildren :: [CategoryId]
                                   , ocPath :: TS.Text
                                   } deriving (Generic, Show)
instance FromJSON OfferCategory where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = camelcaseStripPrefix })

data FilterOption = FilterOption { foCategories :: [CategoryId]
                                 , foRanges :: [Int]
                                 , foConstraints :: Map TS.Text TS.Text -- {"type":["string"|"integer"|"float"]}
                                 } deriving (Generic, Show)
instance FromJSON FilterOption where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = camelcaseStripPrefix })
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
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = camelcaseStripPrefix })

type FilterParamName = TS.Text

data CategoryFilter = CategoryFilter { cfType :: TS.Text -- enum
                                     , cfLabel :: TS.Text
                                     , cfUnit :: Maybe TS.Text
                                     , cfValues :: [FilterEnumValue]
                                     , cfOptions :: [FilterOption]
                                     } deriving (Generic, Show)
instance FromJSON CategoryFilter where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = camelcaseStripPrefix })

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

type RegionId = Word
type CityId = Word
type DistrictId = Word
data JsonRegion = JsonRegion { jrId :: RegionId
                             , jrName :: TS.Text
                             , jrNormalizedName :: TS.Text -- encoded characters - utf8?
                             } deriving (Generic, Show)
instance FromJSON JsonRegion where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = fieldnameToSnakecase })

data JsonCity = JsonCity { jcId :: CityId
                         , jcName :: TS.Text
                         , jcNormalizedName :: TS.Text
                         , jcHasDistricts :: Bool
                         } deriving (Generic, Show)
instance FromJSON JsonCity where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = fieldnameToSnakecase })

data JsonDistrict = JsonDistrict { jdId :: DistrictId
                                 , jdName :: TS.Text -- encoded
                                 -- TODO geographic location
                                 } deriving (Generic, Show)
instance FromJSON JsonDistrict where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = fieldnameToSnakecase })

data Region = Region { regionName :: TS.Text
                     , regionNormalizedName :: TS.Text
                     , regionCities :: Map CityId City
                     } deriving (Show)
data City = City { cityName :: TS.Text
                 , cityNormalizedName :: TS.Text
                 , cityDistricts :: Maybe (Map DistrictId District)
                 } deriving (Show)
data District = District { districtName :: TS.Text
                         } deriving (Show)

separateRecordTree :: (Foldable f, Functor f, forall a. Monoid (f a)) => (r -> f r0) -> (r -> r0 -> r') -> f r -> f r'
separateRecordTree extractFieldList singleValueConstructor = fold . fmap (
        \r -> fmap (\r0 -> singleValueConstructor r r0) $ extractFieldList r
    )

-- String for compatibility w/ Aeson's fieldLabelModifier
fieldnameToSnakecase :: String -> String
fieldnameToSnakecase = Casing.quietSnake . camelcaseStripPrefix

camelcaseStripPrefix :: String -> String
camelcaseStripPrefix str = uncapitalize stripped
    where stripped = dropWhile isLower str
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
type LocationStruct = Map RegionId Region

parseFilterJson :: JsonString -> [(FilterParamName, [CategoryFilter])]
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

createFilters :: JsonString -> FilterStruct
createFilters = convertCategoryFilter . flattenParsedFilters . parseFilterJson

getCategoryFilters :: FilterStruct -> CategoryId -> [Filter]
getCategoryFilters fs cid = filter (\(fi, _) -> cid `elem` (fiCategories fi)) fs

parseCategoriesJson :: JsonString -> Map String OfferCategory
parseCategoriesJson jsonBStr = case Aeson.eitherDecode jsonBStr of
    Left err -> error $ "Failed to parse categories with the following message: " ++ err
    Right m  -> m

createCategories :: JsonString -> CategoryStruct 
createCategories = Map.mapKeys (read :: String -> Word) . parseCategoriesJson

getCategory :: CategoryStruct -> CategoryId -> Maybe OfferCategory
getCategory = flip Map.lookup

parseLocations :: forall location. (FromJSON location, Show location) => JsonString -> [location]
parseLocations jsonBStr = case (Aeson.eitherDecode jsonBStr :: Either String (Map TS.Text [location])) of
    Left err -> error $ "Failed to parse locations with the following message: " ++ err
    Right m  -> 
        case Map.lookup "data" m of
            Just locationList -> locationList
            Nothing         -> error $ "Parsed location successfully but \"data\" key was not found\n\
                                       \Parsed JSON:\n\n" ++ (take 200 $ show m)

parseRegions :: JsonString -> [JsonRegion]
parseRegions = parseLocations
parseCities :: JsonString -> [JsonCity]
parseCities = parseLocations
parseDistricts :: JsonString -> [JsonDistrict]
parseDistricts = parseLocations

-- TODO refactor this section
-- Need conversion of each level to be more self similar
-- If possible abstract out the recursive descent to a generic function
-- Also, if this separation from IO is the best model
jsonDistrictToDistrict :: JsonDistrict -> District
jsonDistrictToDistrict jdistrict = District { districtName = jdName jdistrict }

jsonCityToCity :: (Map CityId [JsonDistrict]) -> JsonCity -> City
jsonCityToCity mjdistricts (JsonCity cid name normalizedName hasDistricts) = 
    City { cityName = name
         , cityNormalizedName = normalizedName
         , cityDistricts =
            if hasDistricts then Nothing
            else do
                jdistricts <- Map.lookup cid mjdistricts
                let districtsList = map convertDistrict jdistricts
                return (Map.fromList districtsList)
         }
    where convertDistrict jdistrict = (jdId jdistrict, jsonDistrictToDistrict jdistrict)


jsonRegionToRegion :: (Map CityId [JsonDistrict]) -> (Map RegionId [JsonCity]) -> JsonRegion -> Region
jsonRegionToRegion mjdistricts mjcities (JsonRegion rid name normalizedName) = 
    Region { regionName = name
           , regionNormalizedName = normalizedName
           , regionCities = case Map.lookup rid mjcities of
                Just jcities -> Map.fromList $ map convertCity jcities
                Nothing      -> error $ "Failed to get list of JsonCities. Region ID is: " ++ show rid
           }
    where convertCity jcity = (jcId jcity, jsonCityToCity mjdistricts jcity)

createLocationTree :: (Map CityId [JsonDistrict]) -> (Map RegionId [JsonCity]) -> [JsonRegion] -> LocationStruct
createLocationTree mjdistricts mjcities jregions = Map.fromList $ map convertRegion jregions
    where convertRegion jregion = (jrId jregion, jsonRegionToRegion mjdistricts mjcities jregion)

-- TODO how to make sure these structures are not recomputed?