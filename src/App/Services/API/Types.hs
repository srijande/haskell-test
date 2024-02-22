
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}


module App.Services.API.Types where

import Servant
import Data.Aeson
import GHC.Generics

data ResponseWrapper = ResponseWrapper {
  status :: String,
  message :: String,
  result :: Value
} deriving (Show, Generic)

instance ToJSON ResponseWrapper
instance FromJSON ResponseWrapper

type API = "decode" :> ReqBody '[JSON] String :> Post '[JSON] ResponseWrapper
