{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

module Sociabl.SimpleChatroom
       () where

import Data.Typeable
import Control.Monad 

import qualified Sociabl.Actor as SA

data SimpleChatroom = SimpleChatroom { messages :: [Message],
                                       name :: String }
                      deriving (Show, Eq)

instance SA.MonadActor SimpleChatroom where
  handleMessage a m = do
    success <- SA.subscriberHandler m
    if success then return a else
      case m of 
        (SA.BoundActorMessage f (cast -> Just nm@(Message _ _)) _ _) ->
          handleNewMessage a f nm
        (SA.BoundActorMessage f (cast -> Just GetSimpleChatroomModel) _ _) ->
          handleGetSimpleChatroomModel a f
        _ -> do
          SA.throwExceptionA $ SA.ActorException SA.NoMessageHandlerException
          return a


handleGetSimpleChatroomModel :: SimpleChatroom -> 
                                SA.UniqueId -> 
                                SA.Actor SimpleChatroom
handleGetSimpleChatroomModel a f = do
  sub <- SA.isSubscriberA (SA.Subscriber f)
  if sub 
    then undefined
    else SA.throwExceptionA $ SA.ActorException NotASubscriberException
  return a

handleNewMessage :: SimpleChatroom 
                 -> SA.UniqueId
                 -> Message
                 -> SA.Actor SimpleChatroom
handleNewMessage a@(SimpleChatroom { messages = ms, name = n }) f 
  nm@(Message { authorId = aid, msg = m }) = do
  handleNewMessage' f aid nm m
  return $ a { messages = nm:ms }
    where 
      handleNewMessage' f a nm m = do  
        SA.whenA (m == "") 
          (SA.throwExceptionA $ SA.ActorException EmptyStringMessageException)
        SA.whenA (f /= a)
          (SA.throwExceptionA $ 
          SA.ActorException AuthorDoesNotMatchSenderException)
        sub <- SA.isSubscriberA (SA.Subscriber f)
        SA.whenA (not sub) 
          (SA.throwExceptionA $ SA.ActorException NotASubscriberException)
        SA.sendMessageA (SA.AllSubscribersExcept [f]) nm

data SimpleChatroomException = EmptyStringMessageException
                             | AuthorDoesNotMatchSenderException
                             | NotASubscriberException
                             deriving (Show, Eq, Typeable)
            
data Message = Message { authorId :: SA.UniqueId,
                         msg :: String }
                       deriving (Show, Eq, Typeable)
                                
data OtherSimpleChatroomMessage = 
  GetSimpleChatroomModel
  deriving (Show, Eq, Typeable)
           
data SimpleChatroomResponse = SimpleChatroomModel
                              {}

-- Build a generalized Concept of a Model
                                      


