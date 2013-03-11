module Sociabl.SubscriberModel.Tests
       ( tests )
       where

import Data.Hashable
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Sociabl.SubscriberModel

import qualified Data.HashMap as HM

tests :: [Test]
tests = [ testGroup "create model"
          [ testStructuredDataNode1 
          , testStructuredDataNode2 
          , testStructuredDataNode3
          , testStructuredDataNode3'
          , testStructuredDataNode4
          , testStructuredDataNode5
          , testStructuredDataNode6
          , testStructuredDataNode7
          , testStructuredDataNode8
          , testStructuredDataNode9
          , testStructuredDataNode10
          , testStructuredDataNode11 ] 
        , testGroup "test get for model"
          [ testModelGet1 
          , testModelGet2 
          , testModelGet3 ]
        , testGroup "update model with messages"
          [ testUpdateModelWithMessages1
          , testUpdateModelWithMessages2 
          , testUpdateModelWithMessages3
          , testUpdateModelWithMessages4
          , testUpdateModelWithMessages5
          , testUpdateModelWithMessages6
          , testUpdateModelWithMessages7
          , testUpdateModelWithMessages8
          , testUpdateModelWithMessages9
          , testUpdateModelWithMessages10
          , testUpdateModelDeleteArrayElement1
          , testUpdateModelDeleteArrayElement2
          , testUpdateModelDeleteArrayElement3
          , testUpdateModelDeleteArrayElement4
          , testUpdateModelWithMessages11
          , testUpdateModelWithMessages12
          , testUpdateModelWithMessages13
          , testUpdateModelWithMessages14
          , testUpdateModelWithMessages15 
          , testUpdateModelWithMessages16 ]
        , testGroup "required and optional specs"
          [ testModelRequirementsAndOptionals1 
          , testModelRequirementsAndOptionals2 ]
        , testGroup "non-string keys and values"
          [ testNonStringKeysAndValues1 ]
        , testGroup "subscribers and updateModelWithMessages"
          [ testAddSubscribers
          , testAddMultipleSubscribers
          , testAddRemoveSubscribers
          , testAddSubscriberTwice
          , testAddSubscriberIncorrectViewModel
          , testAddSubscribers2
          , testAddSubscribers2optional
          , testAddSubscriberWildcard1
          , testAddSubscriberWildcard2
          , testAddSubscriberIncorrectViewModel2
          , testRemoveNonExistingSubscriber
          ]
        , testGroup "update model with subscribers"
          [ testUpdateModelWithSubscribers
          , testUpdateModelWithSubscribersButNoMatchingSubscriber 
          , testUpdateModelTwiceWithSubscribers 
          , testUpdateModelTwiceWithMultipleSubscribers ]
        , testGroup "reset model with subscribers"
          [ testResetModelWithOneSubscriber 
          , testResetModelWithThreeSubscribers ]
        , testGroup "model client" 
          [ testHandleResetModelClient 
          , testHandleUpdateModelClient 
          , testHandleUpdateWithGapModelClient 
          , testHandleUpdateWithGapModelClient2 
          , testHandleUpdateModelClientChannel 
          , testHandleUpdateModelClientArray 
          , testHandleUpdateModelClientMap ]
        , testGroup "test model client get"
          [ testModelClientGet1 
          , testModelClientGet2 
          , testModelClientGet3 ]
        ]
        
testStructuredDataNode1 :: Test
testStructuredDataNode1 = testCase "create a simple node model" $ do
  let spec = opt node
      spec2 = req node
      model = node' "test model"
  case createModel spec model
       :: Either SubscriberModelException (Model String String) of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)
  case createModel spec2 model
       :: Either SubscriberModelException (Model String String) of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)
    
testStructuredDataNode2 :: Test
testStructuredDataNode2 = testCase "node spec and model do not match" $ do
  let spec = req node
      model = channel' ["test model"]
  case createModel spec model
       :: Either SubscriberModelException (Model String String) of
    Right _ -> assertFailure "Model should not match spec"
    Left e -> ModelDoesNotMatchSpecSpecException @=? e
      
testStructuredDataNode3 :: Test
testStructuredDataNode3 = testCase "create a simple channel node model" $ do
  let spec = req $ channel 3
      model = channel' ["0", "1", "2", "3", "4"]
  case createModel spec model
       :: Either SubscriberModelException (Model String String) of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)
    
    
testStructuredDataNode3' :: Test
testStructuredDataNode3' = testCase "fail to create a simple channel spec" $ do
  let spec = req $ channel 0
      model = channel' ["0", "1", "2", "3", "4"]
  case createModel spec model
       :: Either SubscriberModelException (Model String String) of
    Right _ -> assertFailure ("should get an model does not match spec " ++ 
                              "exception, because the channel has a size of 0")
    Left e -> ModelDoesNotMatchSpecSpecException @=? e
    
testStructuredDataNode4 :: Test
testStructuredDataNode4 = 
  testCase "channel node spec and model do not match" $ do
    let spec = req $ channel 3
        model = node' "some node"
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> assertFailure "Model should not match spec"
      Left e -> ModelDoesNotMatchSpecSpecException @=? e

testStructuredDataNode5 :: Test
testStructuredDataNode5 = 
  testCase "create a simple array node model" $ do
    let spec = req array
        model = array' ["hi", "how", "are", "you"]
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> True @=? True
      Left e -> assertFailure (show e)
      
testStructuredDataNode6 :: Test
testStructuredDataNode6 = 
  testCase "array node spec and model do not match" $ do
    let spec = req array
        model = node' "some node"
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> assertFailure "Model should not match spec"
      Left e -> ModelDoesNotMatchSpecSpecException @=? e

testStructuredDataNode7 :: Test
testStructuredDataNode7 = 
  testCase "create a simple map node model" $ do
    let spec = dict [ ("A", req node)
                    , ("B", req array) ]
        model = dict' [ ("A", node' "Jean")
                      , ("B", array' ["hi", "how", "are", "you"]) ]
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> True @=? True
      Left e -> assertFailure (show e)
      
testStructuredDataNode8 :: Test
testStructuredDataNode8 = 
  testCase "map spec and model do not match" $ do
    let spec = dict [ ("A", req node)
                    , ("B", req array) ]
        model = node' "anything"
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> assertFailure "Model should not match spec"
      Left e -> ModelDoesNotMatchSpecSpecException @=? e

testStructuredDataNode9 :: Test
testStructuredDataNode9 = 
  testCase "map spec and model do not match because of key" $ do
    let spec = dict [ ("A", req node)
                    , ("B", req array) ]
        model = dict' [ ("A", node' "Jean")
                      , ("C", array' ["hi", "how", "are", "you"]) ]
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> assertFailure "Model should not match spec"
      Left e -> ModelDoesNotMatchSpecSpecException @=? e

testStructuredDataNode10 :: Test
testStructuredDataNode10 = 
  testCase "create a simple map node model with wildcard" $ do
    let spec = req $ wildcard $ req node
        model = dict' [ ("A", node' "Jean")
                      -- , ("B", array' ["hi", "how", "are", "you"]) 
                      ]
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> True @=? True
      Left e -> assertFailure (show e)
      
testStructuredDataNode11 :: Test
testStructuredDataNode11 = 
  testCase ("map node model with wildcard" ++ 
            " but internal type does not match") $ do
    let spec = req $ wildcard $ req node
        model = dict' [ ("A", node' "Jean")
                      , ("B", array' ["hi", "how", "are", "you"]) ]
    case createModel spec model
         :: Either SubscriberModelException (Model String String) of
      Right _ -> assertFailure "Model should not match spec"
      Left e -> ModelDoesNotMatchSpecSpecException @=? e

testModelGet1 :: Test
testModelGet1 = testCase "test get node" $ do 
  Just "Jean" @=? getMNode tm1 ["A"] 
  Nothing @=? getMNode tm1 ["B"]
  Nothing @=? getMNode tm1 ["C"]
  Nothing @=? getMNode tm1 ["Z"]
  Just "Brisk" @=? getMNode tm1 ["D", "B"]
  
testModelGet2 :: Test
testModelGet2 = testCase "test get channel" $ do 
  Just ["1", "2"] @=? getMChannel tm1 ["B"]
  Nothing @=? getMChannel tm1 ["A"] 
  Nothing @=? getMChannel tm1 ["C"] 
  Nothing @=? getMChannel tm1 ["Z"] 
  
testModelGet3 :: Test
testModelGet3 = testCase "test get array" $ do 
  Just ["a", "b", "c"] @=? getMArray tm1 ["C"]
  Nothing @=? getMArray tm1 ["A"] 
  Nothing @=? getMArray tm1 ["B"] 
  Nothing @=? getMArray tm1 ["Z"] 

testSpec1 :: ModelSpec String
testSpec1 = rdict [ ("A", req node)
                  , ("B", req $ channel 2)
                  , ("C", req array)
                  , ("D", rdict [ ("A", opt node)
                                , ("B", req node)
                                , ("C", opt node) ] ) 
                  , ("D2", opt $ wildcard $ opt node) ]

testModel1 :: ModelNode String String
testModel1 = dict' [ ("A", node' "Jean")
                   , ("B", channel' ["1", "2", "3", "4", "5", "6"])
                   , ("C", array' ["a", "b", "c"])
                   , ("D", dict' [ ("A", node' "Tamara")
                                 , ("B", node' "Brisk") ]) ]

tm1 :: Model String String
tm1 = case createModel testSpec1 testModel1 of
  Right m -> m
  Left e -> error (show e)

testUpdateModelWithMessages1 :: Test      
testUpdateModelWithMessages1 =
  testCase "update model node" $ do
    let expectedResult = 
          dict' [ ("A", node' "monkey")
                , ("B", channel' ["1", "2", "3", "4", "5", "6"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["A"] (nnode "monkey") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> assertFailure (show e)
      Left e -> assertFailure (show e)

testUpdateModelWithMessages2 :: Test      
testUpdateModelWithMessages2 =
  testCase "update model channel" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["z", "1", "2"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
        tm1' = case createModel testSpec1 expectedResult of
          Right er -> er
          Left e -> error (show e)
    case updateModel tm1 ["B"] (channeli "z") of
      Right (m', _) -> tm1' @=? m'
      Left e -> assertFailure (show e)
    let expectedResult2 = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["y", "z"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1' ["B"] (channeli "y") of
      Right (m', _) -> case createModel testSpec1 expectedResult2 of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)

testUpdateModelWithMessages3 :: Test      
testUpdateModelWithMessages3 =
  testCase "update model array insert beginning" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["z", "a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayi 0 "z") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages4 :: Test      
testUpdateModelWithMessages4 =
  testCase "update model array insert middle" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "z", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayi 1 "z") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages5 :: Test      
testUpdateModelWithMessages5 =
  testCase "update model array insert end" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "c", "z"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayi 3 "z") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages6 :: Test      
testUpdateModelWithMessages6 =
  testCase "update model array insert out of bounds check" $ do
    case updateModel tm1 ["C"] (arrayi (-1) "z") of
      Right _ -> error "should be out of bounds"
      Left e -> IndexOutOfBoundsException @=? e
    case updateModel tm1 ["C"] (arrayi 4 "z") of
      Right _ -> error "should be out of bounds"
      Left e -> IndexOutOfBoundsException @=? e

testUpdateModelWithMessages7 :: Test      
testUpdateModelWithMessages7 =
  testCase "update model array overwrite beginning" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["z", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayo 0 "z") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages8 :: Test      
testUpdateModelWithMessages8 =
  testCase "update model array overwrite middle" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "z", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayo 1 "z") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages9 :: Test      
testUpdateModelWithMessages9 =
  testCase "update model array overwrite end" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "z"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayo 2 "z") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)

testUpdateModelWithMessages10 :: Test      
testUpdateModelWithMessages10 =
  testCase "update model array overwrite out of bounds check" $ do
    case updateModel tm1 ["C"] (arrayo (-1) "z") of
      Right _ -> error "should be out of bounds"
      Left e -> IndexOutOfBoundsException @=? e
    case updateModel tm1 ["C"] (arrayo 3 "z") of
      Right _ -> error "should be out of bounds"
      Left e -> IndexOutOfBoundsException @=? e
      
testUpdateModelDeleteArrayElement1 :: Test
testUpdateModelDeleteArrayElement1 =
  testCase "delete array element at beginning" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayd 0) of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelDeleteArrayElement2 :: Test
testUpdateModelDeleteArrayElement2 =
  testCase "delete array element at middle" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayd 1) of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)

testUpdateModelDeleteArrayElement3 :: Test
testUpdateModelDeleteArrayElement3 =
  testCase "delete array element at end" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["C"] (arrayd 2) of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelDeleteArrayElement4 :: Test
testUpdateModelDeleteArrayElement4 = 
  testCase "delete model array overwrite out of bounds check" $ do
    case updateModel tm1 ["C"] (arrayd (-1)) of
      Right _ -> error "should be out of bounds"
      Left e -> IndexOutOfBoundsException @=? e
    case updateModel tm1 ["C"] (arrayd 3) of
      Right _ -> error "should be out of bounds"
      Left e -> IndexOutOfBoundsException @=? e
      
testUpdateModelWithMessages11 :: Test      
testUpdateModelWithMessages11 =
  testCase "update model in map" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Monkey")
                              , ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["D", "A"] (nnode "Monkey") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages12 :: Test      
testUpdateModelWithMessages12 =
  testCase "update model in map but wrong key" $
    case updateModel tm1 ["E", "A"] (nnode "Monkey") of
      Right _ -> error "there should be an unknown key exception"
      Left e -> UnknownKeyException @=? e

testUpdateModelWithMessages13 :: Test      
testUpdateModelWithMessages13 =
  testCase "remove optional key from model" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("B", node' "Brisk") ]) ]
    case updateModel tm1 ["D"] (rmkey "A") of
      Right (m', _) -> case createModel testSpec1 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
testUpdateModelWithMessages14 :: Test      
testUpdateModelWithMessages14 =
  testCase "remove key not in spec" $
    case updateModel tm1 ["D"] (rmkey "E") of
      Right _ -> error "there should be an unknown key exception"
      Left e -> UnknownKeyException @=? e
      
testUpdateModelWithMessages15 :: Test      
testUpdateModelWithMessages15 =
  testCase "remove key not in model" $
    case updateModel tm1 ["D"] (rmkey "C") of
      Right _ -> error "there should be an unknown key exception"
      Left e -> UnknownKeyException @=? e
      
testUpdateModelWithMessages16 :: Test
testUpdateModelWithMessages16 =
  testCase "update model in wildcard" $ do
    let expectedResult = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) 
                , ("D2", dict' [("Foo", node' "Bar") ]) ]
        expectedResult2 = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) 
                , ("D2", dict' [("Foo", node' "Bar")
                               , ("Foo2", node' "Bar2") ]) ]
        expectedResult3 = 
          dict' [ ("A", node' "Jean")
                , ("B", channel' ["1", "2"])
                , ("C", array' ["a", "b", "c"])
                , ("D", dict' [ ("A", node' "Tamara")
                              , ("B", node' "Brisk") ]) 
                , ("D2", dict' [("Foo", node' "Bar3") ]) ]

        cm' = case createModel testSpec1 expectedResult of
          Right m -> m
          Left e -> error (show e)
    case updateModel tm1 ["D2", "Foo"] (nnode "Bar") of
      Right (m', _) -> cm' @=? m'
      Left e -> error (show e)
    case updateModel cm' ["D2", "Foo2"] (nnode "Bar2") of
      Right (m', _) -> case createModel testSpec1 expectedResult2 of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
    case updateModel cm' ["D2", "Foo"] (nnode "Bar3") of
      Right (m', _) -> case createModel testSpec1 expectedResult3 of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)

testModelRequirementsAndOptionals1 :: Test
testModelRequirementsAndOptionals1  = 
  testCase "missing required key" $ do
    let ts1' = rdict [ ("R1", req node)
                     , ("O1", opt node) ]
        tm1' = dict' [ ("O1", node' "Tamara") ]
    case createModel ts1' tm1' of
      Right _ -> error "should be model does not match spec exception"
      Left e -> ModelDoesNotMatchSpecSpecException @=? e
     
testModelRequirementsAndOptionals2 :: Test
testModelRequirementsAndOptionals2 = 
  testCase "missing optional key" $ do
    let ts1' = rdict [ ("R1", req node)
                     , ("O1", opt node) ]
        tm1' = dict' [ ("R1", node' "Tamara") ]
    case createModel ts1' tm1' of
      Right _ -> True @=? True
      Left e -> error (show e)

data TestKey = A | B
             deriving (Ord, Show, Eq)
                      
instance Hashable TestKey where
  hash tk = hash $ show tk
  
data TestValue = SomeValue1 
               | SomeValue2
               deriving (Show, Eq)
                
testSpec2 :: ModelSpec TestKey
testSpec2 = rdict [ (A, req node)
                  , (B, req node) ]

testModel2 :: ModelNode TestKey TestValue
testModel2 = dict' [ (A, node' SomeValue1)
                   , (B, node' SomeValue2) ]

tm2 :: Model TestKey TestValue
tm2 = case createModel testSpec2 testModel2 of
  Right m -> m
  Left e -> error (show e)

testNonStringKeysAndValues1 :: Test
testNonStringKeysAndValues1 = 
  testCase "non-string keys and values model and spec" $ do
    let expectedResult = 
          dict' [ (A, node' SomeValue2)
                , (B, node' SomeValue2) ]
    case updateModel tm2 [A] (nnode SomeValue2) of
      Right (m', _) -> case createModel testSpec2 expectedResult of
        Right er -> er @=? m'
        Left e -> error (show e)
      Left e -> error (show e)
      
subscriberTestModel :: Model String String
subscriberTestModel = case createModel (req node) (node' "value") of
  Right m -> m
  Left e -> error (show e)

s0 :: Subscriber String
s0 = createSubscriber 0 vleaf

s1 :: Subscriber String
s1 = createSubscriber 1 vleaf

s2 :: Subscriber String
s2 = createSubscriber 2 vleaf

s3 :: Subscriber String
s3 = createSubscriber 3 vleaf

twoParamUnsafeFn :: Show a => 
                    (t -> t1 -> Either a t2) -> t -> t1 -> t2
twoParamUnsafeFn f p1 p2 = 
  case f p1 p2 of
    Right x -> x
    Left e -> error (show e)

unsafeAddSubscriber :: Model String a -> Subscriber String -> Model String a
unsafeAddSubscriber = twoParamUnsafeFn addSubscriber

unsafeRemoveSubscriber :: Model String a -> Int -> Model String a
unsafeRemoveSubscriber = twoParamUnsafeFn removeSubscriber

isSubMapHelper :: Eq a => Model a b -> Int -> Bool
isSubMapHelper = isSubscriber

isNotSubMapHelper :: Eq a => Model a b -> Int -> Bool
isNotSubMapHelper m = not . isSubscriber m

testAddSubscribers :: Test
testAddSubscribers = 
  testCase "add subscriber" $ do
  let m' = unsafeAddSubscriber subscriberTestModel s0 
  isSubscriber m' 0 @=? True
  isSubscriber m' 1 @=? False
  
unsafeAddManySubscribers :: Model String a -> [Subscriber String] -> 
                            [Model String a]
unsafeAddManySubscribers m [] = [m]
unsafeAddManySubscribers m (s:[]) = 
  [unsafeAddSubscriber m s]
unsafeAddManySubscribers m (s:ss) = 
  let m' = unsafeAddSubscriber m s
  in m' : unsafeAddManySubscribers m' ss

testAddMultipleSubscribers :: Test
testAddMultipleSubscribers =
  testCase "add multiple subscriber" $ do
  let [m', m'', m''', m''''] = 
        unsafeAddManySubscribers subscriberTestModel [s0, s1, s2, s3]
  all (isSubMapHelper m') [0] @=? True
  all (isSubMapHelper m'') [0, 1] @=? True
  all (isSubMapHelper m''') [0, 1, 2] @=? True
  all (isSubMapHelper m'''') [0, 1, 2, 3] @=? True
  all (isNotSubMapHelper m''') [3] @=? True
  all (isNotSubMapHelper m'') [2, 3] @=? True
  all (isNotSubMapHelper m') [1, 2, 3] @=? True
  
testAddRemoveSubscribers :: Test
testAddRemoveSubscribers = 
  testCase "add and remove multiple subscriber" $ do
  let [_, _, m'''] = 
        unsafeAddManySubscribers subscriberTestModel [s0, s1, s2]
      m'''' = unsafeRemoveSubscriber m''' 1    
      m''''' = unsafeRemoveSubscriber m'''' 2
  all (isSubMapHelper m'''') [0, 2] @=? True
  all (isSubMapHelper m''''') [0] @=? True
  all (isNotSubMapHelper m'''') [1] @=? True
  all (isNotSubMapHelper m''''') [1, 2] @=? True
testAddSubscriberTwice :: Test
testAddSubscriberTwice = 
  testCase "add subscriber twice" $ do
  let m' = unsafeAddSubscriber subscriberTestModel s0 
  case addSubscriber m' s0 of
    Right _ -> error "should have returned SubscriberAlreadyExistsException"
    Left e -> e @=? SubscriberAlreadyExistsException
   
testAddSubscriberIncorrectViewModel :: Test
testAddSubscriberIncorrectViewModel = 
  testCase "add subscriber with incorrect view model" $ do
  let badViewSub = createSubscriber 0 $ vmap [("a", vleaf)]
  case addSubscriber subscriberTestModel badViewSub of
    Right _ -> error "should have returned InvalidModelViewException"
    Left e -> e @=? InvalidModelViewException
    
subscriberTestModel2 :: Model String String
subscriberTestModel2 = case createModel 
                            (rdict [ ("A", req node)
                                   , ("B", req node) ]) 
                            (dict' [ ("A", node' "Jean")
                                   , ("B", node' "Tamara") ]) of
                         Right m -> m
                         Left e -> error (show e)
  
subscriberTestModel2optional :: Model String String
subscriberTestModel2optional = case createModel 
                                    (dict [ ("A", req node)
                                          , ("B", req node) ]) 

                                    (dict' [ ("A", node' "Jean")
                                           , ("B", node' "Tamara") ]) of
                                 Right m -> m
                                 Left e -> error (show e)


subscriberTestModel3 :: Model String String
subscriberTestModel3 = case createModel 
                            (req $ wildcard $ req node)
                            (dict' [ ("A", node' "Jean")
                                   , ("B", node' "Tamara") 
                                   , ("C", node' "Greg")]) of
                         Right m -> m
                         Left e -> error (show e)
  
subscriberTestModel4 :: Model String String
subscriberTestModel4 = case createModel 
                            (opt $ wildcard $ req node)
                            (dict' [ ("A", node' "Jean")
                                   , ("B", node' "Tamara") 
                                   , ("C", node' "Greg")]) of
  Right m -> m
  Left e -> error (show e)
    
testAddSubscribers2 :: Test
testAddSubscribers2 = 
  testCase "add subscriber with complex view model" $ do
  let goodViewSub = createSubscriber 0 $ vmap [ ("A", vleaf)
                                             , ("B", vleaf) ]
  case addSubscriber subscriberTestModel2 goodViewSub of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)
    
testAddSubscribers2optional :: Test
testAddSubscribers2optional = 
  testCase "add subscriber with (optional) complex view model" $ do
  let goodViewSub = createSubscriber 0 $ vmap [ ("A", vleaf)
                                             , ("B", vleaf) ]
  case addSubscriber subscriberTestModel2optional goodViewSub of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)

testAddSubscriberWildcard1 :: Test
testAddSubscriberWildcard1 =
  testCase "add a subscriber to a model that has a required Wildcard" $ do
  let goodViewSub = createSubscriber 0 $ vmap [ ("A", vleaf)
                                             , ("B", vleaf) ]
  case addSubscriber subscriberTestModel3 goodViewSub of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)
    
testAddSubscriberWildcard2 :: Test
testAddSubscriberWildcard2 =
  testCase "add a subscriber to a model that has an optional Wildcard" $ do
  let goodViewSub = createSubscriber 0 $ vmap [ ("A", vleaf)
                                             , ("B", vleaf) ]
  case addSubscriber subscriberTestModel4 goodViewSub of
    Right _ -> True @=? True
    Left e -> assertFailure (show e)
    
testAddSubscriberIncorrectViewModel2 :: Test
testAddSubscriberIncorrectViewModel2 = 
  testCase "add subscriber with complex but incorrect model view" $ do
  let 
      badViewSub = createSubscriber 0 $ vmap [ ("A", vleaf)
                                             , ("C", vleaf) ]
  case addSubscriber subscriberTestModel2 badViewSub of
    Right _ -> error "should have returned InvalidModelViewException"
    Left e -> e @=? InvalidModelViewException

testRemoveNonExistingSubscriber :: Test
testRemoveNonExistingSubscriber = 
  testCase "remove a subscriber which was never added in the first place" $
  case removeSubscriber subscriberTestModel 100 of
    Right _ -> error "should have returned NoSuchSubscriberException"
    Left e -> e @=? NoSuchSubscriberException
      
testUpdateModelWithSubscribers :: Test
testUpdateModelWithSubscribers =
  testCase "update model with one subscriber" $ do
  let sub = createSubscriber 1001 $ vmap [ ("A", vleaf) ]
      m' = unsafeAddSubscriber subscriberTestModel2 sub 
  case updateModel m' ["A"] (nnode "Monkey") of
    Right (_, mums) -> 
      mums @=? [ModelUpdateMessage 1001 2 ["A"] (nnode "Monkey")]
    Left e -> error $ show e
    
testUpdateModelWithSubscribersButNoMatchingSubscriber :: Test
testUpdateModelWithSubscribersButNoMatchingSubscriber =
  testCase "update model with no matching subscriber" $ do
  let sub = createSubscriber 1001 $ vmap [ ("B", vleaf) ]
      m' = unsafeAddSubscriber subscriberTestModel2 sub 
  case updateModel m' ["A"]  (nnode "Monkey") of
    Right (_, mums) -> mums @=? []
    Left e -> error $ show e

testUpdateModelTwiceWithSubscribers :: Test
testUpdateModelTwiceWithSubscribers =
  testCase "update model with one subscriber twice" $ do
  let sub = createSubscriber 1001 $ vmap [ ("A", vleaf) ]
      m' = unsafeAddSubscriber subscriberTestModel2 sub 
  case updateModel m' ["A"] (nnode "Monkey") of
    Right (m'', _) -> 
      case updateModel m'' ["A"] (nnode "Boom") of
        Right (_, mums') -> 
          mums' @=? [ModelUpdateMessage 1001 3 ["A"] (nnode "Boom")]
        Left e -> error $ show e
    Left e -> error $ show e

testUpdateModelTwiceWithMultipleSubscribers :: Test
testUpdateModelTwiceWithMultipleSubscribers =
  testCase "update model with multiple subscribers twice" $ do
  let sub0 = createSubscriber 1000 $ vmap [ ("A", vleaf) ]
      sub1 = createSubscriber 1001 $ vmap [ ("B", vleaf) ]
      sub2 = createSubscriber 1002 $ vmap [ ("A", vleaf) ]
      m' = unsafeAddSubscriber subscriberTestModel2 sub0 
      m'' = unsafeAddSubscriber m' sub1 
      m''' = unsafeAddSubscriber m'' sub2 
  case updateModel m''' ["A"] (nnode "Monkey") of
    Right (m'''', _) -> 
      case updateModel m'''' ["A"] (nnode "Boom") of
        Right (_, mums') -> 
          mums' @=? [ ModelUpdateMessage 1000 3 ["A"] (nnode "Boom")
                    , ModelUpdateMessage 1002 3 ["A"] (nnode "Boom") ]
        Left e -> error $ show e
    Left e -> error $ show e
    
testResetModelWithOneSubscriber :: Test
testResetModelWithOneSubscriber =
  testCase "reset a model that has one subscriber" $ do
    let sub = createSubscriber 1001 $ vmap [ ("A", vleaf) 
                                           , ("C", vleaf) ]
        m' = unsafeAddSubscriber tm1 sub
    case resetModel m' testModel1 of
      Right (Model _ _ subs, msg:[]) -> do
        HM.toList subs @=? [(1001, Subscriber 1001 (ModelView 2 
                                                    (vmap [ ("A", vleaf) 
                                                          , ("C", vleaf) ])))]
        msg @=? ModelResetMessage 1001 2
           (dict' [ ("A", node' "Jean")
                  , ("C", array' ["a", "b", "c"]) ])
      Right r -> error $ show r
      Left e -> error $ show e
      
testResetModelWithThreeSubscribers :: Test
testResetModelWithThreeSubscribers =
  testCase "reset a model that has three subscribers" $ do
    let sub0 = createSubscriber 1001 $ vmap [ ("A", vleaf) 
                                           , ("C", vleaf) ]
        sub1 = createSubscriber 1002 $ vmap [ ("A", vleaf) 
                                            , ("D", vleaf) ] 
        sub2 = createSubscriber 1003 $ vmap [ ("A", vleaf) 
                                            , ("D", vmap [ ("A", vleaf) ] ) ]
        m' = unsafeAddSubscriber tm1 sub0
        m'' = unsafeAddSubscriber m' sub1
        m''' = unsafeAddSubscriber m'' sub2
    case resetModel m''' testModel1 of
      Right (Model _ _ subs, msgs) -> do
        subs @=? HM.fromList [ (1001, Subscriber 1001 
                                      (ModelView 2 (vmap [ ("A", vleaf) 
                                                         , ("C", vleaf) ])))
                             , (1002, Subscriber 1002 
                                      (ModelView 2 (vmap [ ("A", vleaf)
                                                         , ("D", vleaf) ])))
                             , (1003, Subscriber 1003
                                      (ModelView 2 
                                       (vmap [ ("A", vleaf)
                                             , ("D", vmap [ ("A", vleaf) ] )]))) ]
        True @=? elem (ModelResetMessage 1001 2 
                       (dict' [ ("A", node' "Jean")
                              , ("C", array' ["a", "b", "c"]) ])) msgs
        True @=? elem (ModelResetMessage 1002 2 
                       (dict' [ ("A", node' "Jean")
                              , ("D", dict' [ ("A", node' "Tamara")
                                            , ("B", node' "Brisk") ]) ])) msgs
        True @=? elem (ModelResetMessage 1003 2
                       (dict' [ ("A", node' "Jean")
                              , ("D", dict' [ ("A", node' "Tamara") ]) ])) msgs
      Right r -> error $ show r
      Left e -> error $ show e
      
testHandleResetModelClient :: Test
testHandleResetModelClient = testCase "reset a model client" $ do
  let mc = createModelClient :: ModelClient String String
      reset = ModelResetMessage 1001 0 (node' "hi")
  case handleModelUpdateMessage mc reset of
    Right mc' -> ModelClient 0 [] (node' "hi") @=? mc'
    Left e -> error $ show e
    
testHandleUpdateModelClient :: Test
testHandleUpdateModelClient = testCase "update a model client" $ do
  let mc = createModelClient :: ModelClient String String
      reset = ModelResetMessage 1001 0 (node' "hi")
      update = ModelUpdateMessage 1001 1 [] (nnode "you")
  case handleModelUpdateMessage mc reset of
    Right mc' -> case handleModelUpdateMessage mc' update of
      Right mc'' -> ModelClient 1 [] (node' "you") @=? mc''
      Left e -> error $ show e
    Left e -> error $ show e
    
testHandleUpdateWithGapModelClient :: Test
testHandleUpdateWithGapModelClient = 
  testCase "update a model client with version gap between updates" $ do
  let mc = createModelClient :: ModelClient String String
      reset = ModelResetMessage 1001 0 (node' "hi")
      update1 = ModelUpdateMessage 1001 2 [] (nnode "there")
      update2 = ModelUpdateMessage 1001 1 [] (nnode "you")
  case handleModelUpdateMessage mc reset of
    Right mc' -> case handleModelUpdateMessage mc' update1 of
      Right mc''@(ModelClient 0 _ nodes) -> do
        node' "hi" @=? nodes
        case handleModelUpdateMessage mc'' update2 of
          Right mc''' -> ModelClient 2 [] (node' "there") @=? mc'''
          e -> error $ show e      
      e -> error $ show e
    e -> error $ show e
    
testHandleUpdateWithGapModelClient2 :: Test
testHandleUpdateWithGapModelClient2 = 
  testCase ("update a model client with version gap between " ++ 
            "a reset and an update") $ do
  let mc = createModelClient :: ModelClient String String
      reset1 = ModelResetMessage 1001 0 (node' "hi")
      update = ModelUpdateMessage 1001 2 [] (nnode "there")
      reset2 = ModelResetMessage 1001 1 (node' "you")
  case handleModelUpdateMessage mc reset1 of
    Right mc' -> case handleModelUpdateMessage mc' update of
      Right mc''@(ModelClient 0 _ nodes) -> do
        node' "hi" @=? nodes
        case handleModelUpdateMessage mc'' reset2 of
          Right mc''' -> ModelClient 2 [] (node' "there") @=? mc'''
          e -> error $ show e      
      e -> error $ show e
    e -> error $ show e

testHandleUpdateModelClientChannel :: Test
testHandleUpdateModelClientChannel = 
  testCase "update a model client channel" $ do
    let mc = createModelClient :: ModelClient String String
        reset = ModelResetMessage 1001 0 (channel' ["hi", "you"])
        update = ModelUpdateMessage 1001 1 [] (channeli "brother")
    case handleModelUpdateMessage mc reset of
      Right mc' -> case handleModelUpdateMessage mc' update of
        Right mc'' -> ModelClient 1 [] (channel' ["brother", "hi", "you"])
                      @=? mc''
        Left e -> error $ show e
      Left e -> error $ show e
      
testHandleUpdateModelClientArray :: Test
testHandleUpdateModelClientArray = 
  testCase "update a model client array" $ do
    let mc = createModelClient :: ModelClient String String
        reset = ModelResetMessage 1001 0 (array' ["hi", "you"])
        update = ModelUpdateMessage 1001 1 [] (arrayo 1 "brother")
    case handleModelUpdateMessage mc reset of
      Right mc' -> case handleModelUpdateMessage mc' update of
        Right mc'' -> ModelClient 1 [] (array' ["hi", "brother"])
                      @=? mc''
        Left e -> error $ show e
      Left e -> error $ show e
      
testHandleUpdateModelClientMap :: Test
testHandleUpdateModelClientMap = 
  testCase "update a model client map" $ do
    let mc = createModelClient :: ModelClient String String
        reset = ModelResetMessage 1001 0 (dict' [ ("a", node' "na")
                                                , ("b", node' "nb") ] ) 
        update = ModelUpdateMessage 1001 1 ["a"] (nnode "na'")
    case handleModelUpdateMessage mc reset of
      Right mc' -> case handleModelUpdateMessage mc' update of
        Right mc'' -> ModelClient 1 [] (dict' [ ("a", node' "na'")
                                              , ("b", node' "nb") ] )
                      @=? mc''
        Left e -> error $ show e
      Left e -> error $ show e

tmc1 :: ModelClient String String
tmc1 = ModelClient 0 [] testModel1
      
testModelClientGet1 :: Test
testModelClientGet1 = testCase "test model client get node" $ do 
  Just "Jean" @=? getMCNode tmc1 ["A"] 
  Nothing @=? getMCNode tmc1 ["B"]
  Nothing @=? getMCNode tmc1 ["C"]
  Nothing @=? getMCNode tmc1 ["Z"]
  Just "Brisk" @=? getMCNode tmc1 ["D", "B"]
  
testModelClientGet2 :: Test
testModelClientGet2 = testCase "test model client get channel" $ do 
  Just ["1", "2", "3", "4", "5", "6"] @=? getMCChannel tmc1 ["B"]
  Nothing @=? getMCChannel tmc1 ["A"] 
  Nothing @=? getMCChannel tmc1 ["C"] 
  Nothing @=? getMCChannel tmc1 ["Z"] 
  
testModelClientGet3 :: Test
testModelClientGet3 = testCase "test model client get array" $ do 
  Just ["a", "b", "c"] @=? getMArray tm1 ["C"]
  Nothing @=? getMCArray tmc1 ["A"] 
  Nothing @=? getMCArray tmc1 ["B"] 
  Nothing @=? getMCArray tmc1 ["Z"]

