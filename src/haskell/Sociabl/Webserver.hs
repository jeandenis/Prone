{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings #-}
  
module Sociabl.Webserver
       ( applicationInitializer
       , site ) where

import Control.Applicative
import Snap.Extension
import Snap.Extension.Heist.Impl
import Snap.Util.FileServe
import Snap.Types
import Text.Templating.Heist
import qualified Control.Concurrent.STM as STM

import Sociabl.Actor

type Application = SnapExtend ApplicationState

data ApplicationState = 
  ApplicationState { templateState :: HeistState Application 
                   , actorManager :: STM.TVar ActorManager }

instance HasHeistState Application ApplicationState where
  getHeistState = templateState
  setHeistState s a = a { templateState = s }

applicationInitializer :: STM.TVar ActorManager -> Initializer ApplicationState
applicationInitializer am = do
  heist <- heistInitializer "resources/templates" id
  return $ ApplicationState heist am

site :: Application ()
site = ifTop index <|> serveDirectory "resources/static"
       
index :: SnapExtend ApplicationState ()
index = heistLocal (bindString "serverVariables" "") $ render "index"

