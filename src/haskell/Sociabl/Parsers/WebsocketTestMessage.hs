{-# LANGUAGE DeriveDataTypeable #-}

module Sociabl.Parsers.WebsocketTestMessage
       ( 
         WebsocketTestMessage (..)
       , WebsocketTestResponse (..)
       ) where

import Data.Typeable

data WebsocketTestMessage = IncrementNumber
                          | ResetNumber Int
                          | SendMessage String
                          | SendMessageBack String
                          deriving (Show, Eq, Typeable)

data WebsocketTestResponse = SuccessNumberResponse Int
                          | ChangeWord String
                           deriving (Show, Eq, Typeable)