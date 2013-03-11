{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

module Sociabl.Actor.Tests
       ( tests )
       where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Typeable
import Data.Maybe
import Data.Aeson
import qualified Control.Monad.STM as STMM
import qualified Control.Concurrent.STM as STM
import qualified Data.IntMap as IntMap
import qualified Control.Concurrent as Conc

import Sociabl.Actor
import Sociabl.SubscriberModel

tests :: [Test]
tests = [ testGroup "create an Actor and send messages" 
          [ testCreateActor1 
          , testCreateActor1bis
          , testCreateActor2 
          , testCreateActor3
          , testCreateActor4 ]
        , testGroup "actor state"
          [ testActorState1 ]
        , testGroup "actor actions and exceptions"
          [ testActorActions1
          , testActorActions2
          , testActorActions3
          , testActorActions4
          , testActorActions5 ]
        , testGroup "actors communicating with actors"
          [ testActorToActor1 
          , testActorToActor2 
          , testActorToActor2bis 
          , testActorToActor2bis2
          , testActorToActor6
          , testActorToActor7
          , testActorToActor8 ]
        , testGroup "actors and models"
          [ testActorAndModels1 
          , testActorAndModels2 
          , testActorAndModels3 ]
        ]
        
actorManager :: IO (STM.TVar ActorManager)
actorManager = STMM.atomically $ 
               STM.newTVar ActorManager { actorProxies = IntMap.empty, 
                                          nextUniqueId = 1 }

data Message1 = Message1 Int
              | Message1bis Int
              deriving (Show, Typeable)
                       
instance FromJSON Message1 where 
  parseJSON _ = undefined

instance ToJSON Message1 where toJSON _ = undefined
                       
data Reply1 = Reply1 Int
              deriving (Show, Typeable)

instance FromJSON Reply1 where 
  parseJSON _ = undefined

instance ToJSON Reply1 where toJSON _ = undefined

data TestActor1 = TestActor1

instance MonadActor TestActor1 where
  handleMessage a m = do
    case m of
      (ActorMessage _ (cast -> Just (Message1 i)) _) -> replyA $ Reply1 i
      (ActorMessage _ (cast -> Just (Message1bis i)) _) -> 
        replyTransactionalA $ Reply1 i
      (ActorMessage _ (cast -> Just InitActor) _) -> return ()
      _ -> error "incorrect message"
    return a

testCreateActor1 :: Test
testCreateActor1 = testCase "create actor, send message, get response" $ do
  am <- actorManager
  apx <- createActor am TestActor1
  rep <- (apx !!> Message1 5) 10000
  case rep of
    Just (ActorResponse (cast -> (Just (Reply1 i)))) -> 5 @=? i
    _ -> error "incorrect response"

testCreateActor1bis :: Test
testCreateActor1bis = testCase ("create actor, send message, get response " ++
                                "via transactional response") $ do
  am <- actorManager
  apx <- createActor am TestActor1
  rep <- (apx !!> Message1bis 5) 10000
  case rep of
    Just (ActorResponse (cast -> (Just (Reply1 i)))) -> 5 @=? i
    _ -> error "incorrect response"

data TestActor2 = TestActor2

instance MonadActor TestActor2 where
  handleMessage _ _ = return TestActor2

testCreateActor2 :: Test
testCreateActor2 = 
  testCase "send message, timeout bc no response after 10000 microsecs" $ do
    am <- actorManager
    apx <- createActor am TestActor2
    rep <- (apx !!> Message1 0) 10000
    case rep of
      Nothing -> True @=? True
      _ -> error "incorrect response"
      
testCreateActor3 :: Test
testCreateActor3 = 
  testCase "send messages to multiple actors" $ do
    am <- actorManager
    apx <- createActor am TestActor1
    rep0 <- (apx !!> Message1 0) 10000
    rep1 <- (apx !!> Message1 1) 10000
    rep2 <- (apx !!> Message1 2) 10000
    rep3 <- (apx !!> Message1 3) 10000
    case rep0 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 0 @=? i
      _ -> error "incorrect response"
    case rep1 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 1 @=? i
      _ -> error "incorrect response"
    case rep2 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 2 @=? i
      _ -> error "incorrect response"
    case rep3 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 3 @=? i
      _ -> error "incorrect response"
      
testCreateActor4 :: Test
testCreateActor4 = 
  testCase "create multiple actors with different unique ids" $ do
    am <- actorManager
    apx0 <- createActor am TestActor2
    apx1 <- createActor am TestActor2
    apx2 <- createActor am TestActor2
    apx3 <- createActor am TestActor2
    apx4 <- createActor am TestActor2
    False @=? uid apx0 `elem` map uid [apx1, apx2, apx3, apx4]
    False @=? uid apx1 `elem` map uid [apx0, apx2, apx3, apx4]
    False @=? uid apx2 `elem` map uid [apx0, apx1, apx3, apx4]
    False @=? uid apx3 `elem` map uid [apx0, apx1, apx2, apx4]
    False @=? uid apx4 `elem` map uid [apx0, apx1, apx2, apx3]

data TestActor3 = TestActor3 Int

instance MonadActor TestActor3 where
  handleMessage a@(TestActor3 j) m =
    case m of
      (ActorMessage _ (cast -> Just (SetStateM i)) _) ->
        return $ TestActor3 i
      (ActorMessage _ (cast -> Just GetStateM) _) -> do
        replyA $ Reply1 j
        return a
      (ActorMessage _ (cast -> Just InitActor) _) -> return a        
      _ -> error "incorrect message"
             
data SetStateM = SetStateM Int
               deriving (Show, Typeable)

instance FromJSON SetStateM where 
  parseJSON _ = undefined

instance ToJSON SetStateM where toJSON _ = undefined

data GetStateM = GetStateM
               deriving (Show, Typeable)
                        
instance FromJSON GetStateM where 
  parseJSON _ = undefined

instance ToJSON GetStateM where toJSON _ = undefined

getState :: ActorProxy -> IO (Maybe ActorResponse)
getState apx = (apx !!> GetStateM) 10000

testActorState1 :: Test
testActorState1 = 
  testCase "update actor state via messages" $ do
    am <- actorManager
    apx <- createActor am (TestActor3 100)
    rep0 <- getState apx
    apx !> SetStateM 101
    rep1 <- getState apx
    apx !> SetStateM 102
    rep2 <- getState apx
    case rep0 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 100 @=? i
      _ -> error "incorrect response"
    case rep1 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 101 @=? i
      _ -> error "incorrect response"
    case rep2 of
      Just (ActorResponse (cast -> Just (Reply1 i))) -> 102 @=? i
      _ -> error "incorrect response"

data TestActor5 = TestActor5 Int

instance MonadActor TestActor5 where
  handleMessage a@(TestActor5 j) m =
    case m of 
      (ActorMessage _ (cast -> Just Terminate) _) -> do
        queueActorActionsA [TerminateSelf]
        return a
      (ActorMessage _ (cast -> Just (Later when what)) _) -> do
        queueActorActionsA [DoLater when (Message1 what)]
        return a      
      (ActorMessage _ (cast -> Just ThrowExceptionCheckState) _) -> do
        throwExceptionA $ ActorException DefaultException
        return $ TestActor5 100
      (ActorMessage _ (cast -> Just (ReplyThenThrowException i)) _) -> do
        replyA $ Reply1 i
        throwExceptionA $ ActorException DefaultException
        return $ TestActor5 100
      (ActorMessage _ 
       (cast -> Just (TransactionalReplyThenThrowException i)) _) -> do
        replyTransactionalA  $ Reply1 i
        throwExceptionA $ ActorException DefaultException
        return $ TestActor5 100
      (ActorMessage _ (cast -> Just GetStateM) _) -> do
        replyA $ Reply1 j
        return a
      (ActorMessage _ (cast -> Just (Message1 i)) _) -> do
        replyA $ Reply1 i       
        return $ TestActor5 i
      (BoundActorMessage _ (cast -> Just (Message1 i)) _ _) -> do
        replyA $ Reply1 i 
        return $ TestActor5 i        
      (ActorMessage _ (cast -> Just InitActor) _) -> return a
      _ -> error "incorrect message"

data ActionActionMessages = Terminate
                          | Later Int Int
                          | ThrowExceptionCheckState
                          | ReplyThenThrowException Int
                          | TransactionalReplyThenThrowException Int
                          deriving (Show, Typeable)

instance FromJSON ActionActionMessages where 
  parseJSON _ = undefined
  
instance ToJSON ActionActionMessages where 
  toJSON _ = undefined

testActorActions1 :: Test
testActorActions1 = testCase "terminate self" $ do
  am <- actorManager
  apx <- createActor am $ TestActor5 0
  rep1 <- (apx !!> Message1 1) 10000
  apx !> Terminate
  rep2 <- (apx !!> Message1 2) 10000
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 1 @=? i
    _ -> error "incorrect response"
  case rep2 of
    Nothing -> True @=? True
    _ -> error "incorrect response"
  
testActorActions2 :: Test
testActorActions2 = testCase "do later" $ do
  am <- actorManager
  apx <- createActor am $ TestActor5 0
  apx !> Later 1000 5
  rep1 <- getState apx
  Conc.threadDelay 10000
  rep2 <- getState apx
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 0 @=? i
    _ -> error "incorrect response"
  case rep2 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 5 @=? i
    _ -> error "incorrect response"
  
testActorActions3 :: Test
testActorActions3 = testCase "throw exception, check state" $ do
  am <- actorManager
  apx <- createActor am $ TestActor5 0
  rep1 <- getState apx
  apx !> ThrowExceptionCheckState
  rep2 <- getState apx
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 0 @=? i
    _ -> error "incorrect response"
  case rep2 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 0 @=? i
    _ -> error "incorrect response"
    
testActorActions4 :: Test
testActorActions4 = testCase "throw exception, reply, get reply" $ do
  am <- actorManager
  apx <- createActor am $ TestActor5 0
  rep <- (apx !!> ReplyThenThrowException 576) 10000
  case rep of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 576 @=? i  
    _ -> error "incorrect response"
    
testActorActions5 :: Test
testActorActions5 = testCase ("throw exception, transactional reply, " ++ 
                              "not reply") $ do
  am <- actorManager
  apx <- createActor am $ TestActor5 0
  rep <- (apx !!> TransactionalReplyThenThrowException 576) 10000
  case rep of
    Just (ActorResponse (cast -> Just (Reply1 _))) -> error "incorrect response"
    _ ->  True @=? True
  
data TestActor6 = TestActor6 Int (Model String String)

instance MonadActor TestActor6 where
  handleMessage a@(TestActor6 j model) m =
    case m of 
      (ActorMessage _ (cast -> Just GetUniqueId) _) -> do
        i <- getUniqueIdA
        replyA $ Reply1 i
        return a
      (ActorMessage _ (cast -> Just (SendMessageToOtherActor i m')) _) -> do
        apx <- getActorProxyA i
        case apx of 
          Just ap -> 
            ap !-> SetStateM m'
          Nothing -> error "incorrect message"
        return a
      (ActorMessage _ (cast -> Just (SendMessageToOtherActor' i m')) _) -> do
        sendMessageTransactionalA (SpecificActors [i]) (SetStateM m')
        return a   
      (ActorMessage _ (cast -> Just (SendMessageToOtherActor2' i m')) _) -> do
        sendMessageA (SpecificActors [i]) (SetStateM m')
        return a   
      (ActorMessage _ (cast -> Just (CreateAnotherTestActor6 i)) _) -> do
        ap <- newActorA $ TestActor6 i tm1
        replyA $ Reply1 $ uid ap
        return a
      (ActorMessage _ (cast -> Just (TryProxyForNonExistentActor i)) _) -> do
        apx <- getActorProxyA i
        case apx of
          Nothing -> replyA $ Reply1 i
          _ -> error "incorrect message"
        return a
      (BoundActorMessage _ (cast -> Just (SetStateM i)) _ _) -> 
        return $ TestActor6 i model
      (ActorMessage _ (cast -> Just GetStateM) _) -> do
        replyA $ Reply1 j
        return a
      (ActorMessage _ (cast -> Just InitActor) _) -> return a        
      _ -> error "incorrect message"        

data A2AMessages = GetUniqueId
                 | SendMessageToOtherActor UniqueId Int
                 | SendMessageToOtherActor' UniqueId Int
                 | SendMessageToOtherActor2' UniqueId Int
                 | CreateAnotherTestActor6 Int
                 | TryProxyForNonExistentActor UniqueId
                 deriving (Show, Typeable)
                          
instance FromJSON A2AMessages where 
  parseJSON _ = undefined
  
instance ToJSON A2AMessages where 
  toJSON _ = undefined
  
testActorToActor1 :: Test
testActorToActor1 = testCase "get UniqueId" $ do
  am <- actorManager
  apx0 <- createActor am $ TestActor6 0 tm1
  let i' = uid apx0
  rep1 <- (apx0 !!> GetUniqueId) 10000
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> i' @=? i
    _ -> error "incorrect response"

testActorToActor2 :: Test
testActorToActor2 = testCase "send messages between actors" $ do
  am <- actorManager
  apx0 <- createActor am $ TestActor6 0 tm1
  apx1 <- createActor am $ TestActor6 100 tm1
  let i' = uid apx0
  (apx1 !!> SendMessageToOtherActor i' 200) 10000 -- wait for apx1 to process
  rep1 <- getState apx0
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 200 @=? i
    _ -> error "incorrect response"

testActorToActor2bis :: Test
testActorToActor2bis = testCase ("send messages between actors, " ++ 
                                 "using send message transactional") $ do
  am <- actorManager
  apx0 <- createActor am $ TestActor6 0 tm1
  apx1 <- createActor am $ TestActor6 100 tm1
  let i' = uid apx0
  (apx1 !!> SendMessageToOtherActor' i' 200) 10000 -- wait for apx1 to process
  rep1 <- getState apx0
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 200 @=? i
    _ -> error "incorrect response"
    
testActorToActor2bis2 :: Test
testActorToActor2bis2 = testCase ("send messages between actors, " ++ 
                                  "using send message") $ do
  am <- actorManager
  apx0 <- createActor am $ TestActor6 0 tm1
  apx1 <- createActor am $ TestActor6 100 tm1
  let i' = uid apx0
  (apx1 !!> SendMessageToOtherActor2' i' 200) 10000 -- wait for apx1 to process
  rep1 <- getState apx0
  case rep1 of
    Just (ActorResponse (cast -> Just (Reply1 i))) -> 200 @=? i
    _ -> error "incorrect response"

testActorToActor6 :: Test  
testActorToActor6 = testCase "create actor and communicate" $ do
  am <- actorManager
  apx0 <- createActor am $ TestActor6 0 tm1
  rep1 <- (apx0 !!> CreateAnotherTestActor6 1) 10000
  let i1' = case rep1 of
        Just (ActorResponse (cast -> Just (Reply1 i))) -> i
        _ -> error "incorrect response"
  apx1' <- getActorProxy am i1'
  let apx1 = fromMaybe (error "incorrect actor proxy") apx1'
  rep2 <- (apx1 !!> GetStateM) 10000
  case rep2 of
    Just (ActorResponse (cast -> Just (Reply1 i))) ->
      1 @=? i
    _ -> error "incorrect response"
    
testActorToActor7 :: Test
testActorToActor7 = testCase "no proxy for non-existent actor" $ do
  am <- actorManager
  apx0 <- getActorProxy am 5000
  case apx0 of
    Just _ -> error "should not get proxy to non-existent actor"
    Nothing -> True @=? True
    
testActorToActor8 :: Test
testActorToActor8 = testCase "no proxy w/i actor for non-existent actor" $ do
  am <- actorManager
  apx0 <- createActor am $ TestActor6 0 tm1
  res0 <- (apx0 !!> TryProxyForNonExistentActor 9999) 10000
  case res0 of
    Just (ActorResponse (cast -> Just (Reply1 i))) ->
      9999 @=? i
    _ -> error "incorrect response"

data TestActor7 = TestActor7 (Model String String)

instance MonadActor TestActor7 where
  handleMessage (TestActor7 model) m =
    case m of 
      (ActorMessage _ (cast -> Just (TriggerModelUpdate mnc)) _) -> do
        update <- updateModelHelper model ["A", "B"] mnc
        case update of
          Left _ -> error "incorrect update"
          Right model'' -> return $ TestActor7 model''
      (ActorMessage _ (cast -> Just InitActor) _) -> do
        reset <- resetModelHelper model testModel1 
        case reset of
          Left _ -> error "incorrect init"
          Right model' -> return $ TestActor7 model'
      (ActorMessage _ (cast -> Just (AddSubscriber i)) _) -> do
        as <- addSubscriberHelper model 
             (createSubscriber i (vmap [ ("A", vmap [ ("B", vleaf) ] ) ] ) )
        case as of 
          Right model' -> return $ TestActor7 model'
          Left _ -> error "incorrect add subscriber"
      (ActorMessage _ (cast -> Just (RemoveSubscriber i)) _) -> do
        as <- removeSubscriberHelper model i
        case as of
          Right model' -> return $ TestActor7 model'
          Left _ -> error "incorrect remove subscriber"
      _ -> error ("incorrect message" ++ show m)

data TestActor7A = TestActor7A (ModelClient String String)

instance MonadActor TestActor7A where
  handleMessage a@(TestActor7A modelClient) m = do
    handled <- modelClientHelper modelClient m
    case handled of
      Just (Right modelClient') -> return $ TestActor7A modelClient'
      Nothing -> case m of
        (ActorMessage _ (cast -> Just GetModel) _) -> do
            replyA $ ModelResponse modelClient
            return a
        (ActorMessage _ (cast -> Just InitActor) _) -> return a
        _ -> error ("incorrect message" ++ show m)
      _ -> error ("incorrect message" ++ show m)

data TA7Message = TriggerModelUpdate (ModelNodeChange String String)
                | AddSubscriber Int
                | RemoveSubscriber Int
                | GetModel
                  deriving (Show, Typeable)

instance FromJSON TA7Message where 
  parseJSON _ = undefined
  
instance ToJSON TA7Message where 
  toJSON _ = undefined

data TA7Response = ModelResponse (ModelClient String String)
                   deriving (Show, Typeable)
                            
instance FromJSON TA7Response where 
  parseJSON _ = undefined
  
instance ToJSON TA7Response where 
  toJSON _ = undefined

testSpec1 :: ModelSpec String
testSpec1 = rdict [("A", rdict [ ("B", req node) ] )]

testModel1 :: ModelNode String String
testModel1 = dict' [ ("A", dict' [ ("B", node' "Tamara") ]) ]

tm1 :: Model String String
tm1 = case createModel testSpec1 testModel1 of
  Right m -> m
  Left e -> error (show e)

twoParamUnsafeFn :: Show a => (t -> t1 -> Either a t2) -> t -> t1 -> t2
twoParamUnsafeFn f p1 p2 = 
  case f p1 p2 of
    Right x -> x
    Left e -> error (show e)

unsafeAddSubscriber :: Model String a -> Subscriber String -> Model String a
unsafeAddSubscriber = twoParamUnsafeFn addSubscriber

testActorAndModels1 :: Test
testActorAndModels1 = testCase "update model, get messages with model" $ do
  let expectedResult = dict' [ ("A", dict' [ ("B", node' "Jean") ] ) ]
  am <- actorManager
  apx1 <- createActor am $ TestActor7A createModelClient
  apx2 <- createActor am $ TestActor7A createModelClient
  let tv1 = vmap [ ("A", vmap [ ("B", vleaf) ] ) ]
      [i1', i2'] = map uid [apx1, apx2]
      tm1' = unsafeAddSubscriber tm1 $ createSubscriber i1' tv1
      tm1'' = unsafeAddSubscriber tm1' $ createSubscriber i2' tv1
  apx0 <- createActor am $ TestActor7 tm1''
  (apx0 !!> TriggerModelUpdate (nnode "Jean")) 1
  rep1 <- (apx1 !!> GetModel) 10000
  rep2 <- (apx2 !!> GetModel) 10000
  case rep1 of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just expectedResult @=? getModel x
    _ -> error ("incorrect response " ++ show rep1)
  case rep2 of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just expectedResult @=? getModel x
    _ -> error ("incorrect response " ++ show rep2)
  return ()
  
testActorAndModels2 :: Test
testActorAndModels2 = testCase ("update model, get messages with model, " ++  
                                "using addSubscriberHelper") $ do
  let expectedResult = dict' [ ("A", dict' [ ("B", node' "Jean") ] ) ]
  am <- actorManager
  apx1 <- createActor am $ TestActor7A createModelClient
  apx2 <- createActor am $ TestActor7A createModelClient
  let [i1', i2'] = map uid [apx1, apx2]
  apx0 <- createActor am $ TestActor7 tm1
  (apx0 !!> AddSubscriber i1') 10000
  (apx0 !!> AddSubscriber i2') 10000
  rep1 <- (apx1 !!> GetModel) 10000
  rep2 <- (apx2 !!> GetModel) 10000  
  case rep1 of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just testModel1 @=? getModel x
    _ -> error ("incorrect response " ++ show rep1)
  case rep2 of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just testModel1 @=? getModel x
    _ -> error ("incorrect response " ++ show rep2)
  (apx0 !!> TriggerModelUpdate (nnode "Jean")) 1
  rep1' <- (apx1 !!> GetModel) 10000
  rep2' <- (apx2 !!> GetModel) 10000
  case rep1' of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just expectedResult @=? getModel x
    _ -> error ("incorrect response " ++ show rep1)
  case rep2' of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just expectedResult @=? getModel x
    _ -> error ("incorrect response " ++ show rep2)
  return ()
  
testActorAndModels3 :: Test
testActorAndModels3 = testCase ("update model, get messages with model, " ++  
                                "using removeSubscriberHelper") $ do
  let expectedResult = dict' [ ("A", dict' [ ("B", node' "Jean") ] ) ]
  am <- actorManager
  apx1 <- createActor am $ TestActor7A createModelClient
  apx2 <- createActor am $ TestActor7A createModelClient
  let [i1', i2'] = map uid [apx1, apx2]
  apx0 <- createActor am $ TestActor7 tm1
  apx0 !> AddSubscriber i1'
  apx0 !> AddSubscriber i2'
  apx0 !> RemoveSubscriber i2'
  (apx0 !!> TriggerModelUpdate (nnode "Jean")) 1
  rep1 <- (apx1 !!> GetModel) 10000
  rep2 <- (apx2 !!> GetModel) 10000
  case rep1 of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Just expectedResult @=? getModel x
    _ -> error ("incorrect response " ++ show rep1)
  case rep2 of
    Just (ActorResponse (cast -> Just (ModelResponse x))) ->
      Nothing @=? getModel x
    _ -> error ("incorrect response " ++ show rep2)
  return ()

