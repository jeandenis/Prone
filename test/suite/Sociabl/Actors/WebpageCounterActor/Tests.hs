{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

module Sociabl.Actors.WebpageCounterActor.Tests
       ( tests )
       where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Aeson
import Data.Typeable
import Data.Maybe
import qualified Control.Monad.STM as STMM
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson.Types as AET
import qualified Data.Text as DText
import qualified Data.IntMap as IntMap

import Sociabl.Actor
import Sociabl.SubscriberModel
import Sociabl.Actors.WebpageCounterActor

tests :: [Test]
tests = [ testGroup "createActor and add / remove visitors" 
          [ testAddVisitor 
          , testAddVisitors 
          , testRemoveVisitors 
          , testAddVisitorTwice ]
        ]
        
actorManager :: IO (STM.TVar ActorManager)
actorManager = STMM.atomically $ 
               STM.newTVar ActorManager { actorProxies = IntMap.empty, 
                                          nextUniqueId = 1 }
data TestActorMessages = GetCounter 
                       | GetUrl
                       deriving (Show, Eq, Typeable)

instance FromJSON TestActorMessages where 
  parseJSON _ = undefined
  
instance ToJSON TestActorMessages where 
  toJSON _ = undefined

data TestActorResponse = ValueResponse AET.Value
                       deriving (Show, Eq, Typeable)

instance FromJSON TestActorResponse where 
  parseJSON _ = undefined
  
instance ToJSON TestActorResponse where 
  toJSON _ = undefined

data TestActor = TestActor (StandardModelClient)
                 deriving (Show)

instance MonadActor TestActor where
  handleMessage a@(TestActor modelClient) m = do
    handled <- modelClientHelper modelClient m
    case handled of
      Just (Right modelClient') -> return $ TestActor modelClient'
      Nothing -> case m of
        (ActorMessage _ (cast -> Just GetCounter) _) -> do
          replyA $ ValueResponse $ fromJust $ getMCNode modelClient ["counter"]
          return a
        (ActorMessage _ (cast -> Just GetUrl) _) -> do
          replyA $ ValueResponse $ fromJust $ getMCNode modelClient ["url"]
          return a
        (ActorMessage _ (cast -> Just InitActor) _) -> return a
        _ -> error ("incorrect message1 " ++ show m)      
      _ -> error ("incorrect message2 " ++ show m)
        
checkSuccessResponse :: Maybe ActorResponse -> IO ()
checkSuccessResponse rsp =
  case fromJust rsp of
    ActorResponse (cast -> Just SuccessResponse) -> return ()
    _ -> error ("incorrect response " ++ show rsp)
    
checkFailureResponse :: Maybe ActorResponse -> IO ()
checkFailureResponse rsp =
  case fromJust rsp of
    ActorResponse (cast -> Just FailureResponse) -> return ()
    _ -> error ("incorrect response " ++ show rsp)
           
checkValueResponse :: Maybe ActorResponse -> AET.Value -> IO ()
checkValueResponse rsp expectedValue = 
  case fromJust rsp of
    ActorResponse (cast -> Just (ValueResponse s)) ->
      expectedValue @=? s
    _ -> error ("incorrect response " ++ show rsp)
           
testAddVisitor :: Test
testAddVisitor = testCase "create actor, add a visitor, check count" $ do
  am <- actorManager
  wca <- createActor am $ fromJust $ 
         createWebpageCounterActor "http://www.foo.com"
  ta1 <- createActor am $ TestActor createModelClient
  let [i1] = map uid [ta1]
  sr1 <- (wca !!> AddVisitor i1) 10000
  checkSuccessResponse sr1
  rep1 <- (ta1 !!> GetUrl) 10000
  checkValueResponse rep1 (AET.String (DText.pack "http://www.foo.com"))
  rep2 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep2 (AET.Number 1)
  
testAddVisitors :: Test
testAddVisitors = testCase "create actor, add 3 visitors, check count" $ do
  am <- actorManager
  wca <- createActor am $ fromJust $ 
         createWebpageCounterActor "http://www.foo.com"
  ta1 <- createActor am $ TestActor createModelClient
  ta2 <- createActor am $ TestActor createModelClient
  ta3 <- createActor am $ TestActor createModelClient
  let [i1, i2, i3] = map uid [ta1, ta2, ta3]
  sr1 <- (wca !!> AddVisitor i1) 10000
  checkSuccessResponse sr1
  rep1 <- (ta1 !!> GetUrl) 10000
  checkValueResponse rep1 (AET.String (DText.pack "http://www.foo.com"))
  rep2 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep2 (AET.Number 1)
  sr2 <- (wca !!> AddVisitor i2) 10000    
  checkSuccessResponse sr2
  rep1' <- (ta2 !!> GetUrl) 10000
  checkValueResponse rep1' (AET.String (DText.pack "http://www.foo.com"))
  rep3 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep3 (AET.Number 2)
  rep3' <- (ta2 !!> GetCounter) 10000
  checkValueResponse rep3' (AET.Number 2)
  sr3 <- (wca !!> AddVisitor i3) 10000
  checkSuccessResponse sr3
  rep1'' <- (ta3 !!> GetUrl) 10000
  checkValueResponse rep1'' (AET.String (DText.pack "http://www.foo.com"))
  rep4 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep4 (AET.Number 3)
  rep4' <- (ta2 !!> GetCounter) 10000
  checkValueResponse rep4' (AET.Number 3)
  rep4'' <- (ta3 !!> GetCounter) 10000
  checkValueResponse rep4'' (AET.Number 3)
  
testRemoveVisitors :: Test
testRemoveVisitors = testCase ("create actor, add and " ++
                               "remove visitors, check count") $ do
  am <- actorManager
  wca <- createActor am $ fromJust $ 
         createWebpageCounterActor "http://www.foo.com"
  ta1 <- createActor am $ TestActor createModelClient
  ta2 <- createActor am $ TestActor createModelClient
  let [i1, i2] = map uid [ta1, ta2]
  sr1 <- (wca !!> AddVisitor i1) 10000
  checkSuccessResponse sr1
  rep1 <- (ta1 !!> GetUrl) 10000
  checkValueResponse rep1 (AET.String (DText.pack "http://www.foo.com"))
  rep2 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep2 (AET.Number 1)
  sr2 <- (wca !!> AddVisitor i2) 10000    
  checkSuccessResponse sr2
  rep1' <- (ta2 !!> GetUrl) 10000
  checkValueResponse rep1' (AET.String (DText.pack "http://www.foo.com"))
  rep3 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep3 (AET.Number 2)
  rep3' <- (ta2 !!> GetCounter) 10000
  checkValueResponse rep3' (AET.Number 2)
  sr3 <- (wca !!> RemoveVisitor i1) 10000
  checkSuccessResponse sr3
  rep4' <- (ta2 !!> GetCounter) 10000
  checkValueResponse rep4' (AET.Number 1)

testAddVisitorTwice :: Test
testAddVisitorTwice = testCase ("create actor, add a visitor twice, " ++
                                "get a failure response from actor") $ do
  am <- actorManager
  wca <- createActor am $ fromJust $ 
         createWebpageCounterActor "http://www.foo.com"
  ta1 <- createActor am $ TestActor createModelClient
  let [i1] = map uid [ta1]
  sr1 <- (wca !!> AddVisitor i1) 10000
  checkSuccessResponse sr1
  rep1 <- (ta1 !!> GetUrl) 10000
  checkValueResponse rep1 (AET.String (DText.pack "http://www.foo.com"))
  rep2 <- (ta1 !!> GetCounter) 10000
  checkValueResponse rep2 (AET.Number 1)
  sr2 <- (wca !!> AddVisitor i1) 10000
  checkFailureResponse sr2