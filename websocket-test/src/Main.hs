{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings, 
             ViewPatterns, DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Snap.Extension
import Snap.Extension.Heist.Impl
import Snap.Util.FileServe
import Snap.Types
import Text.Templating.Heist
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (forkIO)
import Snap.Extension.Server
import Data.Typeable (cast)
import Data.Maybe (fromJust)
import Data.Typeable

import qualified Data.Aeson.Types as AET
import qualified Data.Attoparsec.Number as AETN
import qualified Data.Text as DText

import Sociabl.WebsocketServer
import Sociabl.Actor
import Sociabl.SubscriberModel
import Sociabl.Logging
import Sociabl.Parsers.WebsocketTestMessage

main :: IO ()
main = do
  initLogger
  am <- createActorManager
  forkIO $ quickHttpServe (applicationInitializer am) site
  let websocketState = WebsocketState am
  quickWebsocketServe websocketState $ handleWebsocket $ TestActor model
  
-- WebServer
--------------------------------------------------------------------------------

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
index = render "index"

-- Actor
--------------------------------------------------------------------------------
  
modelSpec :: StandardModelSpec
modelSpec = rdict [ ("class", req node)
                  , ("word", req node)
                  , ("number", req node) ]
            
visitorModelView :: StandardModelView'
visitorModelView = vmap  [ ("class", vleaf)
                         , ("word", vleaf)
                         , ("number", vleaf) ]
                   
fromRight (Right r) = r

model = 
  fromRight $ createModel modelSpec $ 
  dict' [ ("class", 
           node' (AET.String (DText.pack 
                              ((untilFirstSpace . show . typeOf) 
                               (TestActor undefined)))))
        , ("word", node' (AET.String (DText.pack "sociabl")))
        , ("number", node' (AET.Number 0)) ]

data TestActor = TestActor StandardModel
               deriving (Show, Typeable, Eq)

instance WSActor TestActor where
  handleMessageWS a@(TestActor model) sendMessageToWebsocketClient 
    closeWebsocket msg = do
      case msg of 
        (cast -> Just (ActorMessage _ (cast -> Just InitActor) _)) -> do
          auid <- getUniqueIdA
          model' <- addSubscriberHelper model (createSubscriber auid 
                                               visitorModelView)
          case model' of
            Left _ -> return a
            Right model' -> return $ TestActor model'
        (cast -> Just (ActorMessage _ (cast -> Just IncrementNumber) _)) ->
          getNumber model >>== plusOne model >>>== success model
        (cast -> Just (ActorMessage _ (cast -> Just (ResetNumber n)) _)) ->
          (toMaybe3 (updateModelHelper model ["number"] 
                     (nnode (AET.Number (AETN.I (fromIntegral n)))))) >>>== 
          success2 n model
        (cast -> Just (ActorMessage _ (cast -> Just (SendMessage s)) _)) -> do
          auid <- getUniqueIdA          
          (sendMessageToWebsocketClient
           (BoundActorMessage auid (SendMessageBack s) undefined auid)
           sendMessageBackContinuation)
          return a
        (cast -> Just bm@(BoundActorMessage t m _ f)) -> do
          sendMessageToWebsocketClient bm (\a _ -> return a)
          return a
        _ -> return a
          
sendMessageBackContinuation a@(TestActor model) rsp = do
  case rsp of
    (cast -> Just (ActorResponse (cast -> Just (ChangeWord s)))) -> do
      (toMaybe3 (updateModelHelper model ["word"] 
                 (nnode (AET.String $ DText.pack s)))) >>>== success3 model
    _ -> return a
             
getNumber model = toMaybe1 $ getMNode model ["number"]

plusOne model (AET.Number n) = toMaybe3 $ updateModelHelper model ["number"]
                       (nnode (AET.Number (n + 1)))

success model m = case m of
  Just model' -> do
    replyTransactionalA SuccessResponse
    return $ TestActor model'
  Nothing -> do
    throwExceptionA $ ActorException DefaultException
    return $ TestActor model

success2 num model m = case m of
  Just model' -> do
    replyTransactionalA $ SuccessNumberResponse num
    return $ TestActor model'
  Nothing -> do
    throwExceptionA $ ActorException DefaultException
    return $ TestActor model
    
success3 model m = case m of
  Just model' -> return $ TestActor model'
  Nothing -> do
    throwExceptionA $ ActorException DefaultException
    return $ TestActor model
