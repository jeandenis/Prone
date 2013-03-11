{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module Sociabl.ActorMessageJSON
       ( toActorMessage 
       , AM (..)
       ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Chan
import Data.Typeable
import Data.Aeson 
import Data.Text (unpack)

import qualified Data.Aeson.Types as AET
import qualified Data.Map as DMap

import Sociabl.Actor
import Sociabl.Parsers.JSONParsers()

import qualified Sociabl.Parsers.WebsocketTestMessage as WTM

data AM = forall m. (Show m, Typeable m, ToJSON m) => AM Int m
        | forall m. (Show m, Typeable m, ToJSON m) => BAM Int m Int

instance Eq AM where
  (==) (AM t1 m1) (AM t2 m2) = t1 == t2 && (show m1) == (show m2)
  (==) (BAM t1 m1 f1) (BAM t2 m2 f2) = t1 == t2 && (show m1) == (show m2) && 
                                       f1 == f2
  (==) _ _ = False

instance Show AM where
  show (AM t m) = "AM " ++ (show t) ++ " " ++ (show m)
  show (BAM t m f) = "AM " ++ (show t) ++ " " ++ (show m) ++ " " ++ (show f)
          
instance FromJSON AM where
  parseJSON (Object o) =
    let dataType = DMap.lookup "datatype" o
    in case DMap.lookup "msg" o of
      Just msg ->
        case dataType of
          Just "Sociabl.Actor.ActorDefaultMessage" ->
            createAM o ((parseJSON msg) :: AET.Parser ActorDefaultMessage)
          Just "Sociabl.Parsers.WebsocketTestMessage" -> 
            createAM o ((parseJSON msg) :: AET.Parser WTM.WebsocketTestMessage)
          _ -> mzero
      _ -> mzero
  parseJSON _ = mzero

instance FromJSON ActorResponse where
  parseJSON (Object o) = 
    let dataType = DMap.lookup "datatype" o
    in case DMap.lookup "rsp" o of
      Just rsp ->
        case dataType of
          Just "Sociabl.Actor.ActorDefaultResponse" ->
              ActorResponse <$> (parseJSON rsp 
                                 :: AET.Parser ActorDefaultResponse)
          Just "Sociabl.Parsers.WebsocketTestResponse" -> 
            ActorResponse <$> (parseJSON rsp
                               :: AET.Parser WTM.WebsocketTestResponse)
          _ -> mzero
      _ -> mzero
  parseJSON _ = mzero

instance FromJSON ActorDefaultMessage where
  parseJSON (Object o) = 
    let t = DMap.lookup "type" o
    in case t of
      Just "InitActor" -> return InitActor
      Just "StringMessage" -> 
        case DMap.lookup "value" o of
             Just (AET.String s) -> return $ StringMessage (unpack s)
             _ -> mzero
      _ -> mzero
  parseJSON _ = mzero

instance FromJSON ActorDefaultResponse where
  parseJSON (Object o) = 
    let t = DMap.lookup "type" o
    in case t of
      Just "SuccessResponse" -> return SuccessResponse
      Just "FailureResopnse" -> return FailureResponse
      _ -> mzero
  parseJSON _ = mzero

createAM :: (Typeable a, Show a, ToJSON a) => 
            Object -> AET.Parser a -> AET.Parser AM
createAM o msgParser =
  (BAM <$> o .: "to" <*> msgParser <*> o .: "from") <|>
  (AM <$> o .: "to" <*> msgParser)
  
toActorMessage :: AM -> Chan ActorResponse -> ActorMessage
toActorMessage (AM t m) c = ActorMessage t m c
toActorMessage (BAM t m f) c = BoundActorMessage t m c f
