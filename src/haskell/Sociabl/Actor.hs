{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification,  
             DeriveDataTypeable, ViewPatterns, ScopedTypeVariables, 
             MultiParamTypeClasses, OverloadedStrings #-}

module Sociabl.Actor 
       ( UniqueId
       , MonadActor
       , handleMessage
       , Recipients (..)
       , queueActorActionsA
       , throwExceptionA
       , terminateSelfA
       , getUniqueIdA
       , replyTransactionalA
       , replyA
       , getActorProxyA
       , sendMessageTransactionalA
       , (!->)
       , (!!->)
       , (!!!->)
       , sendMessageA
       , newActorA
       , resetModelHelper
       , updateModelHelper
       , addSubscriberHelper
       , removeSubscriberHelper
       , modelClientHelper
       , Actor()
       , DefaultException (..)
       , ActorException (..)
       , ActorMessage (..)
       , to
       , from
       , ActorDefaultMessage (InitActor, StringMessage)
       , ActorResponse (..)
       , ActorDefaultResponse(..)
       , ActorAction (..)
       , ActorProxy ()
       , uid
       , (!>)
       , (!!>)
       , (!!!>)
       , ActorManager (ActorManager)
       , createActorManager
       , createActorNoInit
       , createActor
       , actorProxies
       , nextUniqueId
       , getActorProxy
       , toMaybe1
       , toMaybe2         
       , toMaybe3         
       , (>>==)
       , (>>>==)
       , untilFirstSpace
       ) where

import Control.Monad
import Data.Typeable
import Data.Hashable
import Control.Concurrent.Chan
import System.Timeout
import Control.Monad.Trans (liftIO)
import Data.Aeson 
import qualified Data.Aeson.Types as AET
import qualified Control.Monad.Writer as MW
import qualified Control.Monad.State as MS
import qualified Control.Monad.Error as ME
import qualified Control.Monad.STM as STMM
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as STM
import qualified Data.IntMap as IntMap

import Sociabl.SubscriberModel

{- MonadActor -}

type UniqueId = Int

class MonadActor a where
  handleMessage :: a -> ActorMessage -> Actor a
  
data Recipients = SpecificActors [UniqueId]
  deriving (Show)
                         
queueActorActionsA :: [ActorAction] -> Actor ()
queueActorActionsA as = Actor $ MW.tell as

throwExceptionA :: ActorException -> Actor ()
throwExceptionA e = Actor $ ME.throwError e

terminateSelfA :: a -> Actor a
terminateSelfA a = do
  queueActorActionsA [TerminateSelf]
  return a

getUniqueIdA :: Actor UniqueId
getUniqueIdA = Actor $ liftM uniqueId MS.get

replyTransactionalA :: (Show r, Typeable r, ToJSON r) => r -> Actor ()
replyTransactionalA r = queueActorActionsA [ReplyTransactional r]

replyA :: (Show r, Typeable r, ToJSON r) => r -> Actor ()
replyA r = Actor $ do
  actorState <- MS.get
  case sender actorState of 
    Sender c -> liftIO $ writeChan c (ActorResponse r)
    _ -> return () -- can never happen
    
getActorProxyA :: UniqueId -> Actor (Maybe ActorProxy)
getActorProxyA id' = Actor $ do 
  actorState <- MS.get
  am <- liftIO . STMM.atomically $ STM.readTVar (manager actorState)  
  return $ case IntMap.lookup id' (actorProxies am) of
    Just apx -> let (ActorProxy i c) = apx in
      Just (BoundActorProxy i c (uniqueId actorState))
    Nothing -> Nothing
    
(!->) :: (Show m, Typeable m, ToJSON m) => ActorProxy -> m -> Actor ()
(!->) apx m = Actor $ liftIO $ apx !> m

(!!->) :: (Show m, Typeable m, ToJSON m) => ActorProxy -> m -> Int ->
         Actor (Maybe ActorResponse)
(!!->) apx m t = Actor $ liftIO $ (apx !!> m) t

(!!!->) :: (Show m, Typeable m, ToJSON m) => 
           ActorProxy -> m -> Actor (Chan ActorResponse)
(!!!->) apx m = Actor $ liftIO $ apx !!!> m

sendMessageTransactionalA :: (Show m, Typeable m, ToJSON m) => 
                             Recipients -> m -> Actor ()
sendMessageTransactionalA (SpecificActors sa) m =
  mapM_ (sendMessage' m) sa
  where 
    sendMessage' m' r' = do
      apx <- getActorProxyA r'
      case apx of 
        Just apx' -> queueActorActionsA [SendMessageTransactional apx' m']
        Nothing -> return ()

sendMessageA :: (Show m, Typeable m, ToJSON m) => Recipients -> m -> Actor ()
sendMessageA (SpecificActors sa) m =
  mapM_ (sendMessage' m) sa
  where
    sendMessage' m' r' = do
      apx <- getActorProxyA r'
      case apx of 
        Just apx' -> liftIO $ apx' !> m'
        Nothing -> return ()

newActorA :: MonadActor a => a -> Actor ActorProxy
newActorA a = do
  actorState <- MS.get
  apx <- liftIO $ createActor (manager actorState) a
  case apx of 
    (ActorProxy i c) -> return $ BoundActorProxy i c (uniqueId actorState)
    (BoundActorProxy i c _) -> 
      return $ BoundActorProxy i c (uniqueId actorState)

resetModelHelper :: (Hashable a, Ord a, Typeable a, Typeable b, ModelKey a,
                     Show a, Show b, ToJSON a, ToJSON b) =>
                    Model a b -> ModelNode a b ->
                    Actor (Either SubscriberModelException (Model a b))
resetModelHelper m mn =
  case resetModel m mn of
    Left e -> return $ Left e
    Right (m', msgs) -> do
      mapM_ (\ms -> case ms of  
               (ModelResetMessage recipient _ _) ->
                 sendMessageTransactionalA (SpecificActors [recipient]) ms
               _ -> return () 
                    -- can never happen because resetModel
                    -- only returns ModelResetMessages
           ) msgs
      return $ Right m' 

updateModelHelper :: (Hashable a, Ord a, Typeable a, ModelKey a,
                      Typeable b, Show a, Show b, ToJSON b) =>
     Model a b -> [a] -> ModelNodeChange a b ->
     Actor (Either SubscriberModelException (Model a b))
updateModelHelper m p mnc =
  case updateModel m p mnc of
    Left e -> return $ Left e
    Right (m', msgs) -> do
      mapM_ (\ms -> case ms of  
               (ModelUpdateMessage recipient _ _ _) -> 
                 sendMessageTransactionalA (SpecificActors [recipient]) ms
               _ -> return ()
                    -- can never happen because resetModel
                    -- only returns ModelUpdateMessages
           ) msgs
      return $ Right m' 

addSubscriberHelper :: (Hashable a, Ord a, Typeable a, Typeable b, ModelKey a,
                        Show a, Show b, ToJSON a, ToJSON b) =>
                       Model a b -> Subscriber a ->
                       Actor (Either SubscriberModelException (Model a b))
addSubscriberHelper model subscriber@(Subscriber i (ModelView ver mv')) = 
  case addSubscriber model subscriber of
    Right model'@(Model _ n _) -> 
      case modelNodesForModelView n mv' of
        Right ns -> do
          sendMessageTransactionalA (SpecificActors [i]) (ModelResetMessage 
                                                          i ver ns)
          return $ Right model'
        Left _ -> 
          return $ Left InvalidModelViewException
    _ -> return $ Left InvalidModelViewException

removeSubscriberHelper :: (Hashable a, Ord a, Typeable a, Typeable b, 
                           Show a, Show b) =>
                          Model a b -> Int -> 
                          Actor (Either SubscriberModelException (Model a b))
removeSubscriberHelper model i = 
  case removeSubscriber model i of
    Right model'@(Model _ _ _) -> do
      sendMessageTransactionalA (SpecificActors [i]) 
        (ModelUnsubscribedMessage i)
      return $ Right model'
    _ -> return $ Left NoSuchSubscriberException


modelClientHelper :: 
  (Hashable a, Ord a, Typeable a, Typeable b, Show a, Show b) =>
  ModelClient a b -> ActorMessage -> 
  Actor (Maybe (Either SubscriberModelException 
                (ModelClient a b)))
modelClientHelper mc (ActorMessage _ 
                      (cast -> Just (ModelUnsubscribedMessage _)) _) =
  return $ Just $ Right $ handleModelUnsubscribedMessage mc
modelClientHelper mc (BoundActorMessage _ 
                      (cast -> Just (ModelUnsubscribedMessage _)) _ _) =
  return $ Just $ Right $ handleModelUnsubscribedMessage mc
modelClientHelper mc (ActorMessage _ (cast -> Just m) _) =
  return $ Just $ handleModelUpdateMessage mc m
modelClientHelper mc (BoundActorMessage _ (cast -> Just m) _ _) =
  return $ Just $ handleModelUpdateMessage mc m
modelClientHelper _ _ = return Nothing

{- Implementation of Monad for Actor -}

newtype Actor a =
  Actor { runA :: MW.WriterT [ActorAction]
                  (MS.StateT ActorState
                   (ME.ErrorT ActorException IO)) a }
  deriving (Monad, ME.MonadIO, MW.MonadWriter [ActorAction],
            MS.MonadState ActorState, ME.MonadError ActorException)
           
runActor :: MonadActor a => 
            Actor a -> ActorState -> 
            IO (Either ActorException (a, ActorState, [ActorAction]))
runActor a as = do 
  r <- runActor' a as
  return $ case r of
    (Right ((a', actions), as')) -> Right (a', as', actions)
    (Left e) -> Left e
  where runActor' a' as' = ME.runErrorT . 
                           flip MS.runStateT as' .
                           MW.runWriterT . 
                           runA $ a'
        
        

{- ActorState -}


-- sender is guaranteed to always refer to the Sender of the message 
-- currently being processed
data ActorState = ActorState { uniqueId :: UniqueId
                             , manager :: STM.TVar ActorManager
                             , sender :: Sender }

instance Show ActorState where
  show as = "ActorState " ++ show (uniqueId as)
                         
data Sender = Sender (Chan ActorResponse)
            | NoSender -- Used for initialization purposes only



{- ActorException -}

data DefaultException = DefaultException 
                      | DefaultExceptionWithMessage String
                      | NoMessageHandlerException 
                      | UniqueIdDoesNotMatchActor
                      deriving (Show, Typeable)
                                                  
data ActorException = forall e. (Show e, Typeable e) => ActorException e

instance Show ActorException where
  show (ActorException s) = show s

instance ME.Error ActorException where
  noMsg = ActorException DefaultException
  strMsg _ = ActorException DefaultException
  
  
  
{- ActorMessage and ActorResponse -}


data ActorMessage = forall m. (Show m, Typeable m, ToJSON m) =>
                    ActorMessage UniqueId m (Chan ActorResponse)
                  | forall m. (Show m, Typeable m, ToJSON m) =>
                    BoundActorMessage UniqueId m (Chan ActorResponse) UniqueId
                  deriving (Typeable)
                    
instance Eq ActorMessage where
  (==) am1 am2 = toJSON am1 == toJSON am2
                           
instance Show ActorMessage where
  show (ActorMessage t m _) =
    "ActorMessage " ++ show t ++ " " ++ show m
  show (BoundActorMessage t m _ f) = 
    "BoundActorMessage " ++ show t ++ " " ++ show m ++ " " ++ show f

to :: ActorMessage -> UniqueId
to (ActorMessage t _ _) = t
to (BoundActorMessage t _ _ _) = t

from :: ActorMessage -> Maybe UniqueId
from (BoundActorMessage _ _ _ f) = Just f
from _ = Nothing

respChannel :: ActorMessage -> Chan ActorResponse
respChannel (ActorMessage _ _ r) = r
respChannel (BoundActorMessage _ _ r _) = r
                                     
data ActorDefaultMessage = InitActor 
                         | StringMessage String
                         deriving (Eq, Typeable, Show)

data ActorResponse = forall r. (Show r, Typeable r, ToJSON r) => 
                     ActorResponse r
                     deriving (Typeable)

instance Eq ActorResponse where
  (==) (ActorResponse r1) (ActorResponse r2) = show r1 == show r2

instance Show ActorResponse where
  show (ActorResponse r) = show r
                     
instance ToJSON ActorMessage where
  toJSON (ActorMessage t msg _) =
    object [ "to" .= t
           , "msg" .= toJSON msg
           , "datatype" .= (untilFirstSpace . show . typeOf) msg]
  toJSON (BoundActorMessage t msg _ f) =
    object [ "to" .= t
           , "from" .= f
           , "msg" .= toJSON msg
           , "datatype" .= (untilFirstSpace . show . typeOf) msg]

instance ToJSON ActorResponse where
  toJSON (ActorResponse r) =
    object [ "datatype" .= (untilFirstSpace . show . typeOf) r
           , "rsp" .= toJSON r]

instance ToJSON ActorDefaultMessage where
  toJSON InitActor = object ["type" .= ("InitActor" :: String)]
  toJSON (StringMessage s) = object ["type" .= ("StringMessage" :: String)
                                    , "value" .= s]

data ActorDefaultResponse = SuccessResponse 
                          | FailureResponse
                          deriving (Eq, Show, Typeable)

instance ToJSON ActorDefaultResponse where
  toJSON SuccessResponse = object ["type" .= ("SuccessResponse" :: String)]
  toJSON FailureResponse = object ["type" .= ("FailureResponse" :: String)]



{- Actor Actions -}

data ActorAction = TerminateSelf
                 | forall m. (Show m, Typeable m, ToJSON m) => 
                   DoLater Int m
                 | forall m. (Show m, Typeable m, ToJSON m) => 
                   SendMessageTransactional ActorProxy m
                 | forall m. (Show m, Typeable m, ToJSON m) => 
                   ReplyTransactional m
                 | NoAction

instance Eq ActorAction where
  (==) TerminateSelf TerminateSelf = True
  (==) NoAction NoAction = True
  (==) _ _ = False

instance Show ActorAction where
  show TerminateSelf = "TerminateSelf"  
  show (DoLater t m) = "DoLater " ++ show t ++ " " ++ show m
  show NoAction = "NoAction"
  show (SendMessageTransactional apx m) = "SendMessageTransactional " ++ 
                                          show apx ++ " " ++ show m
  show (ReplyTransactional m) = "ReplyTransactional " ++ show m


{- Forking Actors -}
                         
startActor :: MonadActor a => STM.TVar ActorManager -> 
              a -> ActorState -> Chan ActorMessage -> IO ()
startActor man a as c = 
  run' a as
  where run' a' as' = do
          m <- readChan c
          let as'' = as' { sender = Sender (respChannel m) }
          x <- processMessage man c a' as'' m
          case x of
            Just (a'', as''') -> run' a'' as'''
            Nothing -> return ()
            
processMessage :: MonadActor a => 
                 STM.TVar ActorManager -> Chan ActorMessage ->
                 a -> ActorState -> ActorMessage -> IO (Maybe (a, ActorState))
processMessage _ c a as m = do
  r <- runActor (handleMessage a m) as
  case r of 
    Right (a', as', actions) -> do 
      mapM_ (processAction c a' as') actions
      if TerminateSelf `elem` actions 
        then return Nothing
        else return $ Just (a', as')
    Left _ -> return $ Just (a, as)
      -- TODO: log exceptions message somewhere
      
      
processAction :: MonadActor a =>
                 Chan ActorMessage -> a -> ActorState -> ActorAction -> IO ()
processAction c _ as action =
  case action of 
    NoAction -> return ()
    TerminateSelf -> return () 
    -- TODO: in case there is cleanup, it should happen here
    DoLater t m -> do 
      Conc.forkIO $ do
        Conc.threadDelay t
        let apx = BoundActorProxy (uniqueId as) c (uniqueId as)
        apx !> m
      return ()
    SendMessageTransactional apx m ->
      apx !> m
    ReplyTransactional r ->
      case sender as of
        Sender ch -> writeChan ch (ActorResponse r)
        _ -> return () -- can never happen


{- ActorManager -}

data ActorManager = 
  ActorManager { actorProxies :: IntMap.IntMap ActorProxy,
                 nextUniqueId :: UniqueId }
  
createActorManager :: IO (STM.TVar ActorManager)
createActorManager = STMM.atomically $ 
                     STM.newTVar ActorManager { actorProxies = IntMap.empty, 
                                                nextUniqueId = 1 }

createActorNoInit :: MonadActor a => STM.TVar ActorManager -> 
                a -> IO ActorProxy
createActorNoInit man a = do
  newChannel <- newChan
  apx <- STMM.atomically $ do
    m <- STM.readTVar man
    let nuid = nextUniqueId m
        apx = ActorProxy nuid newChannel
        aps = IntMap.insert nuid apx (actorProxies m)
    STM.writeTVar man $ m { actorProxies = aps,
                                nextUniqueId = nuid + 1 }
    return apx
  let (ActorProxy i _) = apx
      as = ActorState { uniqueId = i, manager = man, sender = NoSender }
  Conc.forkIO $ startActor man a as newChannel
  return apx

createActor :: MonadActor a => STM.TVar ActorManager -> 
                a -> IO ActorProxy
createActor man a = do
  apx <- createActorNoInit man a
  apx !> InitActor
  return apx
  
getActorProxy :: STM.TVar ActorManager -> IntMap.Key -> 
                 IO (Maybe ActorProxy)
getActorProxy man i = do
  am <- STMM.atomically $ STM.readTVar man
  return $ IntMap.lookup i (actorProxies am) 




{- ActorProxy -}

-- With BoundActorProxy, the second UniqueId is the UniqueId of the 
-- actor which requested an ActorProxy
data ActorProxy = ActorProxy UniqueId (Chan ActorMessage)
                | BoundActorProxy UniqueId (Chan ActorMessage) UniqueId
                  
instance Show ActorProxy where
  show (ActorProxy u _) = "ActorProxy " ++ show u
  show (BoundActorProxy u _ b) = 
    "BoundActorProxy " ++ show u ++ " " ++ show b
  
instance Eq ActorProxy where
  (==) (ActorProxy i1 _) (ActorProxy i2 _) = i1 == i2
  (==) (ActorProxy i1 _) (BoundActorProxy i2 _ _) = i1 == i2
  (==) (BoundActorProxy i1 _ _) (BoundActorProxy i2 _ _) = i1 == i2
  (==) (BoundActorProxy i1 _ _) (ActorProxy i2 _) = i1 == i2
  
uid :: ActorProxy -> UniqueId
uid (ActorProxy i _) = i
uid (BoundActorProxy i _ _) = i
  
(!>) :: (Show m, Typeable m, ToJSON m) => ActorProxy -> m -> IO ()
(!>) apx m = do 
  apx !!!> m
  return ()

(!!>) :: (Show m, Typeable m, ToJSON m) => ActorProxy -> m -> Int ->
         IO (Maybe ActorResponse)
(!!>) apx m t = do
  rc <- apx !!!> m
  timeout t $ readChan rc
                                         
(!!!>) :: (Show m, Typeable m, ToJSON m) => 
          ActorProxy -> m -> IO (Chan ActorResponse)
(!!!>) (ActorProxy u c) m = do 
  rc <- newChan
  writeChan c (ActorMessage u m rc)
  return rc
(!!!>) (BoundActorProxy u c b) m = do 
  rc <- newChan
  writeChan c (BoundActorMessage u m rc b)
  return rc 
  
  
  
{- Actor Maybe and Either Chaining Tools -}

toMaybe1 :: Maybe b -> Actor (Maybe b)
toMaybe1 = return

toMaybe2 :: Either a b -> Actor (Maybe b)
toMaybe2 (Left _) = return Nothing
toMaybe2 (Right r) = return $ Just r

toMaybe3 :: Actor (Either a b) -> Actor (Maybe b)
toMaybe3 a = do
  e <- a
  case e of (Left _) -> return Nothing
            (Right r) -> return $ Just r
  
(>>==) :: Actor (Maybe b) -> (b -> Actor (Maybe c)) -> Actor (Maybe c)
(>>==) a fn = do
  m <- a
  case m of Nothing -> return Nothing
            (Just j) -> fn j

(>>>==) :: Actor (Maybe b) -> (Maybe b -> Actor c) -> Actor c
(>>>==) a fn = do
  m <- a
  fn m

untilFirstSpace :: String -> String
untilFirstSpace [] = []
untilFirstSpace (s:ss)
  | s == ' ' = []
  | otherwise = s:untilFirstSpace ss
  

               
    