module App.Services.Decoder.Utils where

import Data.Time.Format
import Data.Time.Calendar
import Data.Aeson.Key as Key
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import App.Services.Decoder.Types as DecoderTypes

parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str :: Maybe Day

parseValueType :: String -> Bool -> Either String DecoderTypes.ValueType
parseValueType vt isArray = 
  case drop 1 vt of
    "0" -> Right $ Date isArray
    "1" -> Right $ Number isArray
    "2" -> Right $ String isArray
    "3" -> Right $ BoolType isArray
    _ -> Left $ "Error parsing value type: " <> vt

parseIsArray :: String -> Either String Bool
parseIsArray "0" = Right False
parseIsArray "1" = Right True
parseIsArray flag = Left $ "Error parsing value array type: " <> flag

