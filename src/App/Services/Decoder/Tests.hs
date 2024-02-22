module App.Services.Decoder.Tests where

import Control.Monad (when)
import App.Services.Decoder.Main
import App.Services.Decoder.Types
import Test.HUnit


runTests :: IO ()
runTests = do
  testCase1
  >> testCase2
  >> testCase3


testCase1 :: IO ()
testCase1 = do
  let input = "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
  let expectedOutput = "{\"access\":[\"read_db\",\"write_db\",\"view_logs\"],\"age\":20,\"date\":\"1997-02-06\",\"hasPassport\":true,\"name\":\"bob\"}"
  let output = runStringDecryption input
  putStrLn $ "Input: " ++ input
  putStrLn $ "Output: " ++ output
  putStrLn $ "Expected: " ++ expectedOutput

  when (output /= (show expectedOutput)) $ error "Test failed"
  putStrLn "Test passed"

testCase2 :: IO ()
testCase2 = do
  let input = "#00date|199702-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
  let expectedOutput = "{\"access\":[\"read_db\",\"write_db\",\"view_logs\"],\"age\":20,\"date\":\"1997-02-06\",\"hasPassport\":true,\"name\":\"bob\"}"
  let output = runStringDecryption input
  putStrLn $ "Input: " ++ input
  putStrLn $ "Output: " ++ output
  putStrLn $ "Expected: " ++ expectedOutput

  when (output /= (show expectedOutput)) $ error "Test failed"
  putStrLn "Test passed"



testCase3 :: IO ()
testCase3 = do
  let input = "#00date|199702-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"
  let expectedOutput = "{\"access\":[\"read_db\",\"write_db\",\"view_logs\"],\"age\":20,\"date\":\"1997-02-06\",\"hasPassport\":true,\"name\":\"bob\"}"
  let output = runStringDecryption input
  putStrLn $ "Input: " ++ input
  putStrLn $ "Output: " ++ output
  putStrLn $ "Expected: " ++ expectedOutput

  when (output /= (show expectedOutput)) $ error "Test failed"
  putStrLn "Test passed"

