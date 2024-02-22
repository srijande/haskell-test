
module Main (main) where

import Servant
import Network.Wai.Handler.Warp (run)
import App.Services.API.Main as API

data ValueType = Date Bool | Number Bool | String Bool | BoolType Bool deriving Show

main :: IO ()
main = run 8089 API.server
