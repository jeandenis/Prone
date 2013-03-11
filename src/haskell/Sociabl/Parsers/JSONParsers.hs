{-# LANGUAGE OverloadedStrings #-}

module Sociabl.Parsers.JSONParsers
       (
         WebsocketTestMessage (..)
       ) where

import Control.Monad
import Data.Aeson

import qualified Data.Aeson.Types as AET
import qualified Data.Attoparsec.Number as AETN
import qualified Data.Map as DMap
import qualified Data.Text as DText

import Sociabl.Parsers.WebsocketTestMessage

instance FromJSON WebsocketTestMessage where
  parseJSON (Object o) = 
    let t = DMap.lookup "type" o
    in case t of
      Just "IncrementNumber" -> return IncrementNumber
      Just "ResetNumber" -> 
        let resetValue = DMap.lookup "value" o
        in case resetValue of
          Just (AET.Number (AETN.I v)) -> return $ ResetNumber (fromIntegral v)
          _ -> mzero
      Just "SendMessage" ->
        let message = DMap.lookup "value" o
        in case message of
          Just (AET.String s) -> return $ SendMessage (DText.unpack s)
          _ -> mzero
      _ -> mzero
  parseJSON _ = mzero
    
instance ToJSON WebsocketTestMessage where
  toJSON (SendMessageBack s) =
    object [ "type" .= ("SendMessageBack" :: String)
           , "value" .= toJSON s ]
  toJSON _ = undefined -- never used because never sent to client

instance FromJSON WebsocketTestResponse where
  parseJSON (Object o) =
    case DMap.lookup "type" o of
      Just "ChangeWord" ->
        let message = DMap.lookup "value" o
        in case message of
          Just (AET.String s) -> return $ ChangeWord (DText.unpack s)
          _ -> mzero          
      _ -> mzero
  parseJSON _ = mzero -- never used because never get these from client
  
instance ToJSON WebsocketTestResponse where
  toJSON (SuccessNumberResponse i) =
    object [ "type" .= ("SuccessNumberResponse" :: String)
           , "value" .= toJSON i ]
