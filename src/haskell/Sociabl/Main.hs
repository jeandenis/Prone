{-# LANGUAGE ViewPatterns, DeriveDataTypeable, OverloadedStrings,
             ExistentialQuantification, ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Snap.Extension.Server

import Sociabl.Webserver
import Sociabl.WebsocketServer
import Sociabl.Actor
import Sociabl.Logging

data DoNothingWSActor = DoNothingWSActor
                      deriving (Show, Eq)

instance WSActor DoNothingWSActor where
  handleMessageWS wsa _ _ _ = return wsa

main :: IO ()
main = do
  initLogger
  am <- createActorManager
  forkIO $ quickHttpServe (applicationInitializer am) site
  let websocketState = WebsocketState am
  quickWebsocketServe websocketState $ handleWebsocket DoNothingWSActor
  
