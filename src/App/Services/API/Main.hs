module App.Services.API.Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import App.Services.API.Types as APITypes
import qualified App.Services.Decoder.Main as Decoder

decodeHandler :: String -> Handler ResponseWrapper
decodeHandler msg =   
  case Decoder.parseMessage msg of
    Right json -> do
      return $ ResponseWrapper "success" "Decryption successful" $ object json
    Left err -> return $ ResponseWrapper "failure" err Null

api :: Server API
api = decodeHandler

server :: Application
server = serve (Proxy :: Proxy API) api
