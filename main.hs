module Main(main) where

import qualified Data.Aeson as Aeson
import Data.Aeson hiding (String, Number, Bool)
import Data.Aeson.Key as Key
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Format
import Data.Time.Calendar
import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.List (splitAt)
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Scientific

import Test.HUnit 


data ValueType = Date Bool | Number Bool | String Bool | BoolType Bool deriving Show


parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str :: Maybe Day


parseEntity :: String -> Maybe (Aeson.Key, Aeson.Value)
parseEntity str = do
    let valType = take 2 str
    let keyVal = drop 2 str
    let [key, val] = splitOn "|" keyVal
    isArray <- parseIsArray (take 1 valType)
    let valueType = parseValueType valType isArray
    let jsonVal = valueToJSON valueType val
    return ((Key.fromString key), jsonVal)


parseValueType :: String -> Bool -> ValueType
parseValueType vt isArray = 
  case drop 1 vt of
    "0" -> Date isArray
    "1" -> Number isArray
    "2" -> String isArray
    "3" -> BoolType isArray
    _ -> error $ "Error parsing value type" <> vt


parseIsArray :: String -> Maybe Bool
parseIsArray "0" = Just False
parseIsArray "1" = Just True
parseIsArray _ = error $ "Error parsing value array type" 


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


valueToJSON :: ValueType -> String -> Aeson.Value
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



run :: String -> String
run input = 
  case parseMessage input of
    Left err -> "Error: " ++ err
    Right json -> show $ encode $ Aeson.object json



main :: IO ()
main = do
  runTests




runTests :: IO ()
runTests = do
  testCase1
  >> testCase2
  >> testCase3


testCase1 :: IO ()
testCase1 = do
  let input = "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
  let expectedOutput = "{\"access\":[\"read_db\",\"write_db\",\"view_logs\"],\"age\":20,\"date\":\"1997-02-06\",\"hasPassport\":true,\"name\":\"bob\"}"
  let output = run input
  putStrLn $ "Input: " ++ input
  putStrLn $ "Output: " ++ output
  putStrLn $ "Expected: " ++ expectedOutput

  when (output /= (show expectedOutput)) $ error "Test failed"
  putStrLn "Test passed"

testCase2 :: IO ()
testCase2 = do
  let input = "#00date|199702-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
  let expectedOutput = "{\"access\":[\"read_db\",\"write_db\",\"view_logs\"],\"age\":20,\"date\":\"1997-02-06\",\"hasPassport\":true,\"name\":\"bob\"}"
  let output = run input
  putStrLn $ "Input: " ++ input
  putStrLn $ "Output: " ++ output
  putStrLn $ "Expected: " ++ expectedOutput

  when (output /= (show expectedOutput)) $ error "Test failed"
  putStrLn "Test passed"



testCase3 :: IO ()
testCase3 = do
  let input = "#00date|199702-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
  let expectedOutput = "{\"access\":[\"read_db\",\"write_db\",\"view_logs\"],\"age\":20,\"date\":\"1997-02-06\",\"hasPassport\":true,\"name\":\"bob\"}"
  let output = run input
  putStrLn $ "Input: " ++ input
  putStrLn $ "Output: " ++ output
  putStrLn $ "Expected: " ++ expectedOutput

  when (output /= (show expectedOutput)) $ error "Test failed"
  putStrLn "Test passed"

