{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Sociabl.ActorMessageJSON.Tests
       ( tests )
       where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Applicative
import Data.Aeson
import Data.Attoparsec
import Data.Typeable
import qualified Data.Aeson.Types as AET
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Sociabl.Actor
import Sociabl.SubscriberModel
import Sociabl.ActorMessageJSON 

tests :: [Test]
tests = [ testGroup "(Bound)ActorMessage and (B)AM (De)Serialization"
          [ testActorDefaultMessageInit ]
        , testGroup "(Bound)ActorMessage Serialization Only"
          [ testModelResetMessageSimple 
          , testModelResetMessageComplex 
          , testModelResetMessageJSONValues ]
        , testGroup "ActorResponse and (De)Serialization"
          [ testActorDefaultResponseSuccess ]
        ]
        
testAMUtil :: (ToJSON m, Data.Typeable.Typeable m, Show m) =>
              m -> B.ByteString -> B.ByteString -> IO ()
testAMUtil a expected1 expected2 = do
  let am = ActorMessage 1234 a undefined
      bam = BoundActorMessage 1234 a undefined 5678
      s = B.pack $ BL.unpack $ encode am
      bs = B.pack $ BL.unpack $ encode bam
  expected1 @=? s
  expected2 @=? bs
  case parse (fromJSON <$> json) s of
    Done _ (Success (am' :: AM)) -> AM 1234 a @=? am'
    e -> error $ show e
  case parse (fromJSON <$> json) bs of
    Done _ (Success (am' :: AM)) -> BAM 1234 a 5678 @=? am'
    e -> error $ show e

testActorResponse :: (ToJSON r, Typeable r, Show r) =>
                     r -> B.ByteString -> IO ()
testActorResponse r expected = do
  let ar = ActorResponse r
      s = B.pack $ BL.unpack $ encode ar
  expected @=? s
  case parse (fromJSON <$> json) s of
    Done _ (Success (ar' :: ActorResponse)) -> ar @=? ar'
    e -> error $ show e

testActorDefaultMessageInit :: Test
testActorDefaultMessageInit = testCase "for ActorDefaultMessage Init" $ do
  testAMUtil InitActor "{\"datatype\":\"Sociabl.Actor.ActorDefaultMessage\",\"msg\":{\"type\":\"InitActor\"},\"to\":1234}" "{\"datatype\":\"Sociabl.Actor.ActorDefaultMessage\",\"from\":5678,\"msg\":{\"type\":\"InitActor\"},\"to\":1234}"
    
testModelResetMessageSimple :: Test
testModelResetMessageSimple = 
  testCase "for ModelUpdateMessage ModelResetMessage Simple" $ do
    let m = (ModelResetMessage 4 7 (dict' [ ("A", node' "Jean") ] 
                                    :: ModelNode String String))
        am = ActorMessage 1234 m undefined
        bam = BoundActorMessage 1234 m undefined 5678
        sam = B.pack $ BL.unpack $ encode am
        sbam = B.pack $ BL.unpack $ encode bam
        expected1 = "{\"datatype\":\"Sociabl.SubscriberModel.ModelUpdateMessage\",\"msg\":{\"type\":\"ModelResetMessage\",\"uid\":4,\"value\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Jean\"}}},\"version\":7},\"to\":1234}"
        expected2 = "{\"datatype\":\"Sociabl.SubscriberModel.ModelUpdateMessage\",\"from\":5678,\"msg\":{\"type\":\"ModelResetMessage\",\"uid\":4,\"value\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Jean\"}}},\"version\":7},\"to\":1234}"
    expected1 @=? sam
    expected2 @=? sbam

testModelResetMessageComplex :: Test
testModelResetMessageComplex = 
  testCase "for ModelUpdateMessage ModelResetMessage Complex" $ do
    let m = (ModelResetMessage 4 7 (dict' [ ("A", node' "Jean")
                   , ("B", channel' ["1", "2", "3", "4", "5", "6"])
                   , ("C", array' ["a", "b", "c"])
                   , ("D", dict' [ ("A", node' "Tamara")
                                 , ("B", node' "Brisk") ]) ] 
                                    :: ModelNode String String))
        am = ActorMessage 1234 m undefined
        bam = BoundActorMessage 1234 m undefined 5678
        sam = B.pack $ BL.unpack $ encode am
        sbam = B.pack $ BL.unpack $ encode bam
        expected1 = "{\"datatype\":\"Sociabl.SubscriberModel.ModelUpdateMessage\",\"msg\":{\"type\":\"ModelResetMessage\",\"uid\":4,\"value\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Jean\"},\"B\":{\"max\":6,\"type\":\"channel\",\"value\":[\"1\",\"2\",\"3\",\"4\",\"5\",\"6\"]},\"C\":{\"type\":\"array\",\"value\":[\"a\",\"b\",\"c\"]},\"D\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Tamara\"},\"B\":{\"type\":\"node\",\"value\":\"Brisk\"}}}}},\"version\":7},\"to\":1234}"
        expected2 = "{\"datatype\":\"Sociabl.SubscriberModel.ModelUpdateMessage\",\"from\":5678,\"msg\":{\"type\":\"ModelResetMessage\",\"uid\":4,\"value\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Jean\"},\"B\":{\"max\":6,\"type\":\"channel\",\"value\":[\"1\",\"2\",\"3\",\"4\",\"5\",\"6\"]},\"C\":{\"type\":\"array\",\"value\":[\"a\",\"b\",\"c\"]},\"D\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Tamara\"},\"B\":{\"type\":\"node\",\"value\":\"Brisk\"}}}}},\"version\":7},\"to\":1234}"
    expected1 @=? sam
    expected2 @=? sbam

testModelResetMessageJSONValues :: Test
testModelResetMessageJSONValues = 
  testCase "for ModelUpdateMessage ModelResetMessage JSONValues" $ do
    let m = (ModelResetMessage 4 7 
             (dict' [ ("A", node' $ AET.String "Jean")
                    , ("B", channel' [AET.Number 1, AET.Number 2, AET.Number 3])
                    , ("C", array' [AET.String "a", AET.String "b"])
                    , ("D", dict' [ ("A", node' $ AET.Bool False)
                                  , ("B", node' $ AET.Null) 
                                  , ("C", node' $ AET.Array V.empty)
                                  , ("D", node' $ object 
                                          [ ("You", AET.String "Suck")
                                          , ("A", AET.String "Lot")])])] 
              :: StandardModelNode))
        am = ActorMessage 1234 m undefined
        bam = BoundActorMessage 1234 m undefined 5678
        sam = B.pack $ BL.unpack $ encode am
        sbam = B.pack $ BL.unpack $ encode bam
        expected1 = "{\"datatype\":\"Sociabl.SubscriberModel.ModelUpdateMessage\",\"msg\":{\"type\":\"ModelResetMessage\",\"uid\":4,\"value\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Jean\"},\"B\":{\"max\":3,\"type\":\"channel\",\"value\":[1,2,3]},\"C\":{\"type\":\"array\",\"value\":[\"a\",\"b\"]},\"D\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":false},\"B\":{\"type\":\"node\",\"value\":null},\"C\":{\"type\":\"node\",\"value\":[]},\"D\":{\"type\":\"node\",\"value\":{\"A\":\"Lot\",\"You\":\"Suck\"}}}}}},\"version\":7},\"to\":1234}"
        expected2 = "{\"datatype\":\"Sociabl.SubscriberModel.ModelUpdateMessage\",\"from\":5678,\"msg\":{\"type\":\"ModelResetMessage\",\"uid\":4,\"value\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":\"Jean\"},\"B\":{\"max\":3,\"type\":\"channel\",\"value\":[1,2,3]},\"C\":{\"type\":\"array\",\"value\":[\"a\",\"b\"]},\"D\":{\"type\":\"map\",\"value\":{\"A\":{\"type\":\"node\",\"value\":false},\"B\":{\"type\":\"node\",\"value\":null},\"C\":{\"type\":\"node\",\"value\":[]},\"D\":{\"type\":\"node\",\"value\":{\"A\":\"Lot\",\"You\":\"Suck\"}}}}}},\"version\":7},\"to\":1234}"
    expected1 @=? sam
    expected2 @=? sbam

testActorDefaultResponseSuccess :: Test
testActorDefaultResponseSuccess = 
  testCase "for ActorResponse SuccessResponse" $ do
    testActorResponse SuccessResponse "{\"datatype\":\"Sociabl.Actor.ActorDefaultResponse\",\"rsp\":{\"type\":\"SuccessResponse\"}}"

