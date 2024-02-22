{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module App.Services.Decoder.Main where

import Servant
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as Aeson
import Data.Aeson hiding (String, Number, Bool)
import Data.Aeson.Key as Key
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.List (splitAt)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Scientific
import GHC.Generics
import Test.HUnit 
import App.Services.Decoder.Types as DecoderTypes
import App.Services.Decoder.Utils


parseEntity :: String -> Either String (Aeson.Key, Aeson.Value)
parseEntity str = 
  let valType = take 2 str
      keyVal = drop 2 str
      [key, val] = splitOn "|" keyVal
      isArray = parseIsArray (take 1 valType)
  in case isArray of
      Right isArray' ->
        let valueType = parseValueType valType isArray'
        in case valueType of
            Right valueType' -> 
              let jsonVal = valueToJSON valueType' val
                  key' = Key.fromString key
              in
                case jsonVal of
                  Right jsonVal' -> Right (key', jsonVal')
                  Left err -> Left err
            Left err -> Left err
      Left err -> Left err




parseDateToJSON :: String -> Either String Aeson.Value
parseDateToJSON str = 
  case parseDate str of
    Just d -> Right $ Aeson.String (Text.pack $ show d)
    Nothing -> Left $ "Invalid date format: " <> str

parseNumberToJSON :: String -> Either String Aeson.Value
parseNumberToJSON str = do
  let nums = readMaybe str :: Maybe Scientific
  case nums of
    Just num -> Right $ Aeson.Number num
    Nothing -> Left $ "Invalid number format: " <> str

parseStringToJSON :: String -> Either String Aeson.Value
parseStringToJSON str = Right $ Aeson.String $ Text.pack str

parseBoolToJSON :: String -> Either String Aeson.Value
parseBoolToJSON str = Right $ Aeson.Bool (map toLower str `elem` ["y", "t"])

parseArrayToJSON :: (String -> Either String Aeson.Value) -> String -> Either String Aeson.Value
parseArrayToJSON parseFn str = 
  let values = splitOn "," str
      parsedValues = map parseFn values
  in 
    case sequence parsedValues of
      Right values' -> Right $ toJSON values'
      Left err -> Left err

valueToJSON :: DecoderTypes.ValueType -> String -> Either String Aeson.Value
valueToJSON (Date False) = parseDateToJSON
valueToJSON (Number False) = parseNumberToJSON
valueToJSON (String False) = parseStringToJSON
valueToJSON (BoolType False) = parseBoolToJSON
valueToJSON (Date True) = parseArrayToJSON parseDateToJSON
valueToJSON (Number True) = parseArrayToJSON parseNumberToJSON
valueToJSON (String True) = parseArrayToJSON parseStringToJSON
valueToJSON (BoolType True) = parseArrayToJSON parseBoolToJSON

parseMessage :: String -> Either String [(Aeson.Key, Aeson.Value)]
parseMessage str =
  let parts = splitOn "#" str
      entities = map Text.unpack $ filter (not . Text.null) $ map Text.strip $ map (Text.pack) parts
      parsedEntities = map parseEntity entities
  in 
    case sequence parsedEntities of
      Right ents -> Right ents
      Left error -> Left error





runStringDecryption :: String -> String
runStringDecryption input = 
  case parseMessage input of
    Left err -> "Error: " ++ err
    Right json -> show $ encode $ Aeson.object json

