{-# LANGUAGE ViewPatterns, DeriveDataTypeable, OverloadedStrings #-}

module Sociabl.Actors.WebpageCounterActor
       ( createWebpageCounterActor
       , WebPageCounterMessage (..)
       ) where

import Control.Applicative
import Control.Monad
import Data.Aeson 
import Data.Typeable
import qualified Data.Aeson.Types as AET
import qualified Data.Map as DMap
import qualified Data.Text as DText

import Sociabl.Actor
import Sociabl.SubscriberModel

modelSpec :: StandardModelSpec
modelSpec = rdict [ ("url", req node)
                  , ("counter", req node) ]

visitorModelView :: StandardModelView' 
visitorModelView = vmap [ ("url", vleaf)
                        , ("counter", vleaf) ]

data WebPageCounterActor = WebPageCounterActor 
                           (StandardModel)

data WebPageCounterMessage = AddVisitor UniqueId
                           | RemoveVisitor UniqueId
                           deriving (Typeable, Show, Eq)
                          
instance FromJSON WebPageCounterMessage where 
  parseJSON (Object o) = 
    let t = DMap.lookup "type" o
    in case t of
      Just "AddVisitor" -> AddVisitor <$> o .: "uid"
      Just "RemoveVisitor" -> RemoveVisitor <$> o .: "uid"
      _ -> mzero
  parseJSON _ = mzero
  
instance ToJSON WebPageCounterMessage where 
  toJSON (AddVisitor i) = object [ "type" .= ("AddVisitor" :: String)
                                   , "uid" .= i]
  toJSON (RemoveVisitor i) = object [ "type" .= ("RemoveVisitor" :: String)
                                   , "uid" .= i]
                                    
instance MonadActor WebPageCounterActor where
  handleMessage a msg =
    case msg of
      (ActorMessage _ (cast -> Just (AddVisitor i)) _) -> 
        addVisitor a i
      (BoundActorMessage _ (cast -> Just (AddVisitor i)) _ _) -> 
        addVisitor a i
      (ActorMessage _ (cast -> Just (RemoveVisitor i)) _) -> 
        removeVisitor a i
      (BoundActorMessage _ (cast -> Just (RemoveVisitor i)) _ _) -> 
        removeVisitor a i
      (ActorMessage _ (cast -> Just InitActor) _) -> return a
      _ -> do
        throwExceptionA $ ActorException NoMessageHandlerException
        return a

createWebpageCounterActor :: String -> Maybe WebPageCounterActor
createWebpageCounterActor url =
  case createModel modelSpec $ 
       dict' [ ("url", node' (AET.String (DText.pack url)))
             , ("counter", node' (AET.Number 0)) ] of 
    Right model -> Just $ WebPageCounterActor model
    Left _ -> Nothing

addVisitor :: WebPageCounterActor -> Int -> Actor WebPageCounterActor
addVisitor (WebPageCounterActor model) i =
  getCounter model >>== plusOne >>== addSub >>>== success model
  where plusOne = counterChange model 
                  (\counter -> case counter of 
                      (AET.Number n) -> Just $ AET.Number (n + 1)
                      _ -> Nothing)
        addSub model' = toMaybe3 $ addSubscriberHelper model' 
                        (createSubscriber i visitorModelView)

removeVisitor :: WebPageCounterActor -> Int -> Actor WebPageCounterActor
removeVisitor (WebPageCounterActor model) i = 
  getCounter model >>== minusOne >>== removeSub >>>== success model
  where minusOne = counterChange model 
                   (\counter -> case counter of 
                       (AET.Number n) -> Just $ AET.Number (n - 1)
                       _ -> Nothing)
        removeSub model' = toMaybe3 $ removeSubscriberHelper model' i
        
counterPath :: [String]
counterPath = ["counter"]
        
getCounter :: Model String b -> Actor (Maybe b)
getCounter model = toMaybe1 $ getMNode model counterPath

counterChange :: (Typeable b, Show b, ToJSON b) =>
                 Model String b -> (t -> Maybe b) -> t -> 
                 Actor (Maybe (Model String b))
counterChange model fn counter = case (fn counter) of
  Just counter' -> toMaybe3 $ updateModelHelper model counterPath 
                   (nnode counter')
  Nothing -> return Nothing

success :: Model String AET.Value -> 
           Maybe (Model String AET.Value) -> Actor WebPageCounterActor
success model m = case m of 
  Just model' -> do
    replyTransactionalA SuccessResponse 
    return $ WebPageCounterActor model'
  Nothing -> do
    replyA FailureResponse
    throwExceptionA $ ActorException DefaultException
    return $ WebPageCounterActor model -- never get here because of exception