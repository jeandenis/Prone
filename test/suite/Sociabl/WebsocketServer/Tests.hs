{-# LANGUAGE OverloadedStrings #-}

module Sociabl.WebsocketServer.Tests
       ( tests )
       where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Sociabl.Actor
import Sociabl.WebsocketServer

tests :: [Test]
tests = [ testGroup "test the websocket message protocol parser"
          [ testWSMessage 
          , testWSMessageMultiDigitUID
          , testWSMessageNonDigitMessageId
          , testWSMessageNonDigitUId 
          , testWSMessageMissingUId 
          , testWSMessageMissingComponent 
          , testWSResponse 
          , testWSDisconnect
          , testWSDisconnectButMessageBody 
          , testWSMessageUnicode ]
        ]

testWSMessage :: Test
testWSMessage = testCase "test parsing to WSMessage" $ do
  let msg = B.append "1:0:7:!" $ B.pack $ 
            BL.unpack (encode (ActorMessage 7 InitActor undefined))
  (Just $ WSMessage 0 7 InitActor) @=? (websocketParser 7 msg)
  
testWSMessageMultiDigitUID :: Test
testWSMessageMultiDigitUID = 
  testCase "test parsing to WSMessage where uids are multi-digit" $ do 
  let msg = B.append "1:1234:7123:!" $ B.pack $ 
            BL.unpack (encode (ActorMessage 7123 InitActor undefined))
  (Just $ WSMessage 1234 7123 InitActor) @=? (websocketParser 7123 msg)
  
testWSMessageNonDigitMessageId :: Test
testWSMessageNonDigitMessageId =
  testCase "test parsing WSMessage with non-digit message id" $ do 
  let msg = B.append "1:76s8f:7123:!" $ B.pack $ 
            BL.unpack (encode (ActorMessage 7123 InitActor undefined))
  Nothing @=? (websocketParser 7123 msg)

testWSMessageNonDigitUId :: Test
testWSMessageNonDigitUId =
  testCase "test parsing WSMessage with non-digit uid" $ do 
  let msg = B.append "1:2:71d23:!" $ B.pack $ 
            BL.unpack (encode (ActorMessage 7123 InitActor undefined))
  Nothing @=? (websocketParser 7123 msg)

testWSMessageMissingUId :: Test
testWSMessageMissingUId = testCase "test parsing to WSMessage missing uid" $ do
  let msg = B.append "1:0::!" $ B.pack $ 
            BL.unpack (encode (ActorMessage 7 InitActor undefined))
  Nothing @=? (websocketParser 7 msg)
  
testWSMessageMissingComponent :: Test
testWSMessageMissingComponent = 
  testCase "test parsing to WSMessage missing component" $ do
    let msg = B.append "1:0:!" $ B.pack $ 
              BL.unpack (encode (ActorMessage 7 InitActor undefined))
    Nothing @=? (websocketParser 7 msg)
  
testWSResponse :: Test
testWSResponse = testCase "test parsing to WSResponse" $ do
  let msg = B.append "2:0r:7:!" $ B.pack $ 
            BL.unpack (encode (ActorResponse SuccessResponse))
  (Just $ WSResponse 0 7 SuccessResponse) @=? (websocketParser 7 msg)
  
testWSDisconnect :: Test
testWSDisconnect = testCase "test parsing to WSDisconnect" $ do
  let msg = "3:0:7:!"
  (Just $ WSDisconnect) @=? (websocketParser 7 msg)
  
testWSDisconnectButMessageBody :: Test
testWSDisconnectButMessageBody = 
  testCase "test parsing to WSDisconnect, fails because of message body" $ do
    let msg = "3:0:7:!{}"
    Nothing @=? (websocketParser 7 msg)

testWSMessageUnicode :: Test
testWSMessageUnicode = testCase "test parsing to WSMessage with Unicode" $ do
  let msg = B.append "1:0:7:!" $ B.pack $ 
            BL.unpack (encode (ActorMessage 7 
                               (StringMessage "čušpajž日本語") undefined))
  (Just $ WSMessage 0 7 (StringMessage "čušpajž日本語")) @=? 
    (websocketParser 7 msg)