module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Sociabl.Actor.Tests
import qualified Sociabl.SubscriberModel.Tests
import qualified Sociabl.Actors.WebpageCounterActor.Tests
import qualified Sociabl.ActorMessageJSON.Tests
import qualified Sociabl.WebsocketServer.Tests

main :: IO ()
main = defaultMain tests
  where tests = [ 
          testGroup "Sociabl.Actor.Tests" 
          Sociabl.Actor.Tests.tests 
          ,
          testGroup "Sociabl.SubscriberModel.Tests" 
          Sociabl.SubscriberModel.Tests.tests 
          , 
          testGroup "Sociabl.Actors.WebpageCounterActor.Tests"
          Sociabl.Actors.WebpageCounterActor.Tests.tests
          ,
          testGroup "Sociabl.ActorMessageJSON.Tests"
          Sociabl.ActorMessageJSON.Tests.tests
          ,
          testGroup "Sociabl.WebsocketServer.Tests"
          Sociabl.WebsocketServer.Tests.tests
          ]