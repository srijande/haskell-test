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
import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.List (splitAt)
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Scientific
import GHC.Generics
import Test.HUnit 
import App.Services.Decoder.Types as DecoderTypes
import App.Services.Decoder.Utils


parseEntity :: String -> Maybe (Aeson.Key, Aeson.Value)
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
                return (key', jsonVal)
            Left err -> error $ "Error parsing valueType" <> err
      Left err -> error $ "Error parsing isArray" <> err




parseDateToJSON :: String -> Aeson.Value
parseDateToJSON str = 
  case parseDate str of
    Just d -> Aeson.String (Text.pack $ show d)
    Nothing -> error "Invalid date format"

parseNumberToJSON :: String -> Aeson.Value
parseNumberToJSON str = Aeson.Number $ Data.Scientific.scientific (read str) 0

parseStringToJSON :: String -> Aeson.Value
parseStringToJSON str = Aeson.String $ Text.pack str

parseBoolToJSON :: String -> Aeson.Value
parseBoolToJSON str = Aeson.Bool (map toLower str `elem` ["y", "t"])

parseArrayToJSON :: (String -> Aeson.Value) -> String -> Aeson.Value
parseArrayToJSON parseFn str = 
  let values = splitOn "," str
  in 
    toJSON $ map parseFn values

valueToJSON :: DecoderTypes.ValueType -> String -> Aeson.Value
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
      Just ents -> Right ents
      Nothing -> Left $ "Error parsing entities" <> show entities <> show parsedEntities





runStringDecryption :: String -> String
runStringDecryption input = 
  case parseMessage input of
    Left err -> "Error: " ++ err
    Right json -> show $ encode $ Aeson.object json

