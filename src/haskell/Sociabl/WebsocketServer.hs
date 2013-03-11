{-# LANGUAGE ViewPatterns, DeriveDataTypeable, OverloadedStrings,
             ExistentialQuantification, ScopedTypeVariables #-}

module Sociabl.WebsocketServer
       ( WebsocketMessage (WSMessage, WSResponse, WSDisconnect)
       , WebsocketState (WebsocketState)
       , WSActor
       , quickWebsocketServe 
       , handleWebsocket 
       , handleMessageWS
       , websocketParser
       ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative hiding (many)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Attoparsec 
import Data.Attoparsec.Char8 (digit)
import Data.ByteString.UTF8 (toString) 
import Data.Typeable
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import Network.WebSockets (Request, shakeHands, getFrame, putFrame)
import System.IO (Handle, hClose, hFlush)
import System.Log.Logger
import System.Timeout

import qualified Data.Aeson.Types as AET
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BT
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BT8
import qualified Data.ByteString.UTF8 as BTU8
import qualified Control.Monad.STM as STMM
import qualified Data.IntMap as IntMap

import Sociabl.Actor
import Sociabl.ActorMessageJSON
import Sociabl.Logging

type MessageId = Int

data WebsocketState =
  WebsocketState { actorManager :: STM.TVar ActorManager }
  
quickWebsocketServe :: 
  WebsocketState -> (WebsocketState -> Request -> Handle -> IO ()) -> IO a
quickWebsocketServe wss handler =
  withSocketsDo $ do
    socket <- listenOn (PortNumber 8088)
    forever $ do
      (handle, _, _) <- accept socket
      forkIO $ do
        hs <- shakeHands handle
        case hs of
          Right request -> handler wss request handle
          Left err -> warningM defaultLogger $ 
                      "Unsuccessful Websocket Handshake: " ++ show err

data WebsocketMessage = forall m. (Show m, Typeable m, ToJSON m) => 
                        WSMessage MessageId UniqueId m
                      | forall r. (Show r, Typeable r, ToJSON r) => 
                        WSResponse MessageId UniqueId r
                      | WSDisconnect
                      deriving (Typeable)

instance Show WebsocketMessage where
  show (WSMessage i1 u1 m1) =
    "WSMessage " ++ show i1 ++ " " ++ show u1 ++ " " ++ show m1
  show (WSResponse i1 u1 r1) =
    "WSResponse " ++ show i1 ++ " " ++ show u1 ++ " " ++ show r1
  show WSDisconnect = "WSDisconnect"

instance ToJSON WebsocketMessage where
  toJSON _ = AET.Null -- never sent to web-based client

instance Eq WebsocketMessage where
  (==) (WSMessage i1 u1 m1) (WSMessage i2 u2 m2) =
    i1 == i2 && u1 == u2 && (show m1 == show m2)
  (==) (WSResponse i1 u1 r1) (WSResponse i2 u2 r2) =
    i1 == i2 && u1 == u2 && (show r1 == show r2)
  (==) WSDisconnect WSDisconnect = True
  (==) _ _ = False

class WSActor a where
  handleMessageWS :: a -> 
                     (ActorMessage -> Continuation a -> 
                      Actor ()) -> 
                     Actor () ->
                     ActorMessage -> Actor a

data WebsocketActor a = 
  WebsocketActor (STM.TVar MessageId) Handle 
  (STM.TVar (IntMap.IntMap (Continuation a))) a
  
instance (WSActor a) => MonadActor (WebsocketActor a) where
  handleMessage a@(WebsocketActor nextMessageId handle continuations wsa) 
    msg = 
    case msg of
      am@(ActorMessage _ (cast -> Just WSDisconnect) _) -> do
        wsa' <- (handleMessageWS wsa (\_ _ -> return ()) 
                 (return ()) am)
        terminateSelfA $ 
          WebsocketActor nextMessageId handle continuations wsa'
      (ActorMessage _ (cast -> Just (WSResponse messageId aid rsp)) _) -> do
        aid' <- getUniqueIdA
        if aid == aid'
          then do wsa' <- doContinuationA wsa continuations messageId 
                          (ActorResponse rsp)
                  return $ 
                    WebsocketActor nextMessageId handle continuations wsa'
          else do throwExceptionA $ ActorException UniqueIdDoesNotMatchActor
                  return a
      _ -> do
        wsa' <- (handleMessageWS wsa 
                 (sendWebsocketMessageA msg handle
                  nextMessageId continuations) 
                 (liftIO $ hClose handle) msg)
        return $ WebsocketActor nextMessageId handle continuations wsa'

doContinuationA :: a ->
                   (STM.TVar (IntMap.IntMap (Continuation a))) ->
                   Int -> ActorResponse -> Actor a
doContinuationA a continuations messageId rsp = do
  x <- liftIO $ do
    continuation <- STMM.atomically $ do
      cs <- STM.readTVar continuations
      case IntMap.lookup messageId cs of
        Just c -> do
          let cs' = IntMap.delete messageId cs -- can only reply once
          STM.writeTVar continuations cs'
          return $ Just c
        Nothing -> return Nothing
    return continuation
  case x of
    Just c -> c a rsp
    Nothing -> return a

type Continuation a = (a -> ActorResponse -> Actor a)

sendWebsocketMessageA :: ActorMessage ->
                         Handle ->
                         (STM.TVar MessageId) ->
                         (STM.TVar (IntMap.IntMap (Continuation a))) ->
                         ActorMessage -> Continuation a -> 
                         Actor ()
sendWebsocketMessageA msg h nextMessageId continuations  
  msg'@(BoundActorMessage t m _ f) continuation = do
    aid <- getUniqueIdA
    liftIO $ do
      nextId <- STMM.atomically $ do
        nextId <- STM.readTVar nextMessageId
        STM.writeTVar nextMessageId $ nextId + 1
        cs <- STM.readTVar continuations
        STM.writeTVar continuations $ 
          IntMap.insert nextId continuation cs
        return nextId
      let msg'' = case msg == msg' of 
            True -> -- just forwarding a message from another actor
              Just $ msg'
            False -> -- message from the WSActor, make sure no duping
              if aid == f && aid == t 
              then Just $ msg'
              else Nothing
      case msg'' of 
        Just msg''' -> do
          putFrame h $ BT.concat [ "1:"
                                 , BTU8.fromString $ show nextId
                                 , ":"
                                 , BTU8.fromString $ show aid
                                 , ":!"
                                 , BT.pack $ BL.unpack $ 
                                   encode $ msg''' ]
          return ()
        _ -> return ()
    
sendWebsocketMessageA _ _ _ _ _ _ = return ()

handleWebsocket :: WSActor a =>
                   a -> WebsocketState -> Request -> Handle -> IO ()
handleWebsocket a wss request handle = do
  infoM defaultLogger $ "New Websocket Connection: " ++ show request
  continuations <- STMM.atomically $ STM.newTVar IntMap.empty
  nextId <- STMM.atomically $ STM.newTVar 1 -- the init message is 0
  websocketActor <- createActorNoInit (actorManager wss) $ 
        WebsocketActor nextId handle continuations a
  sendWSInitMessage websocketActor handle
  hFlush handle -- get back to client as quickly as possible
  websocketActor !> InitActor
  loop websocketActor
  where loop websocketActor = do
          msg <- getFrame handle
          if BT.null msg 
            then do
            websocketActor !> WSDisconnect
            infoM defaultLogger 
              ("EOF encountered. Closing Websocket Connection: " ++ 
               show request)
            hClose handle
            else do
            infoM defaultLogger ("Received message: " ++ toString msg)
            case websocketParser (uid websocketActor) msg of
              Just m@(WSDisconnect) -> do
                websocketActor !> m
                infoM defaultLogger ("WSDisconnect message received. " ++
                                     "Closing Websocket Connection: ")
                hClose handle
              Just (WSMessage messageId _ m) -> do 
                -- wait 30 secs max for reply
                replyChan <- websocketActor !!!> m
                forkIO $ do
                  reply <- timeout 30000000 $ readChan replyChan
                  case reply of 
                    Just r -> do
                      putFrame handle $ BT.concat 
                        [ "2:"
                        , BTU8.fromString $ show messageId ++ "r"
                        , ":"
                        ,  BTU8.fromString $ show (uid websocketActor)
                        , ":!"
                        , BT.pack $ BL.unpack $ encode r ]
                    Nothing -> return ()
                loop websocketActor
              Just r@(WSResponse _ _ _) -> do
                websocketActor !> r
                loop websocketActor
              Nothing -> do
                errorM defaultLogger ("Could not parse message: " ++ 
                                      toString msg)
                loop websocketActor

sendWSInitMessage :: ActorProxy -> Handle -> IO  ()
sendWSInitMessage websocketActor handle = 
  putFrame handle $ BT.concat [ "0:0:"
                              ,  BTU8.fromString $ show (uid websocketActor)
                              , ":!"]

createWSMessage :: UniqueId ->
                   [Char] -> 
                   [Char] -> 
                   BT8.ByteString -> 
                   Maybe WebsocketMessage
createWSMessage realUniqueId messageId uniqueId message =
   case parse (fromJSON <$> json) message of
     Done _ (Success (AM t m)) ->
             if realUniqueId == t && realUniqueId == (read uniqueId)
             then Just $ WSMessage (read messageId) (read uniqueId) m
             else Nothing
     _ -> Nothing

createWSResponse :: UniqueId -> [Char] -> [Char] -> BT8.ByteString -> 
                    Maybe WebsocketMessage
createWSResponse realUniqueId messageId uniqueId reply =
  case parse (fromJSON <$> json) reply of
    Done _ (Success (ActorResponse r)) ->
      if realUniqueId == (read uniqueId)
      then Just $ WSResponse (read messageId) (read uniqueId) r
      else Nothing
    _ -> Nothing

createWSDisconnect :: Int -> [Char] -> Maybe WebsocketMessage
createWSDisconnect realUniqueId uniqueId =
  if realUniqueId == (read uniqueId) 
  then Just $ WSDisconnect
  else Nothing

websocketParser :: UniqueId -> BT8.ByteString -> Maybe WebsocketMessage
websocketParser i bs = 
  let wsmParser = feed (parse (do
        (skip (inClass "1") *> skip (inClass ":") *> 
         (createWSMessage i <$> many1 digit <* 
          skip (inClass ":") <*> many1 digit <*
          skip (inClass ":") <* 
          skip (inClass "!") <*> takeByteString <* endOfInput))
        <|> (skip (inClass "2") *> skip (inClass ":") *> 
         (createWSResponse i <$> many1 digit <* skip (inClass "r") <*
          skip (inClass ":") <*> many1 digit <*
          skip (inClass ":") <* 
          skip (inClass "!") <*> takeByteString <* endOfInput))
        <|> (skip (inClass "3") *> skip (inClass ":") *> 
             many1 digit *> skip (inClass ":") *>
             (createWSDisconnect i <$> many1 digit <*
              skip (inClass ":") <* 
              skip (inClass "!") <* endOfInput))) bs ) BT8.empty
  in case wsmParser of
    (Done _ m) -> m
    _ -> Nothing