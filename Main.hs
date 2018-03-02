{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Proxy
import Servant
import Protolude hiding (Down)
import Network
import GHC.Generics (Generic)
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import Network.Wai.Handler.Warp (run)

type IP = Text

data Status = Up | Down
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance MimeRender PlainText Status where
  mimeRender _ Up   = "up"
  mimeRender _ Down = "down"

type API = QueryParam "host"     HostName
        :> QueryParam "port"     Int
        :> QueryParam "timeout"  Int
        :> Get '[PlainText,JSON] Status

main :: IO ()
main = do
  port <- maybe (die "Usage: prog PORT") return =<< getPortArg
  run port (serve (Proxy @API) server)


server :: Server API
server (Just host) (Just port) mTimeout = do
  let tout = maybe 5000000 (*1000000) mTimeout
  res <- liftIO $ timeout tout $ tryJust (guard . isDoesNotExistError)
    (connectTo host (PortNumber (fromIntegral port)) >>= hClose)
  return $ case res of
    Just (Right ()) -> Up
    _               -> Down

getPortArg :: IO (Maybe Int)
getPortArg = do
  args <- getArgs
  case args of
    [portStr] | [(port, "")] <- reads portStr -> return (Just port)
    _ -> return Nothing
