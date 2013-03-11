{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, 
             DeriveDataTypeable, ViewPatterns, TypeSynonymInstances, 
             FlexibleContexts, OverloadedStrings, OverlappingInstances, 
             UndecidableInstances, FlexibleInstances #-}

-- regntrixie

module Sociabl.SubscriberModel
       ( SubscriberModelException (..)
       , ModelKey
       , StandardModel
       , StandardModelSpec
       , StandardModelNode
       , StandardModelClient
       , StandardModelView'
       , Model (Model)
       , ModelSpec ()
       , ModelNode ()
       , ModelNodeChange ()
       , Subscriber (..)
       , ModelView (..)
       , ModelView' (..)
       , ModelUpdateMessage (..)
       , ModelUpdateMessage' (..)
       , createModel
       , createSubscriber
       , addSubscriber
       , removeSubscriber
       , isSubscriber
       , updateModel
       , resetModel
       , modelNodesForModelView
       , getMNode
       , getMChannel
       , getMArray                  
       , wildcard
       , dict
       , rdict
       , opt
       , req
       , node
       , channel
       , array
       , node'
       , channel'
       , array'
       , dict'
       , nnode
       , arrayi
       , arrayo
       , arrayd
       , channeli
       , rmkey 
       , vmap
       , vleaf 
       , ModelClient (ModelClient)
       , createModelClient
       , getModel
       , handleModelUnsubscribedMessage
       , handleModelUpdateMessage
       , getMCNode
       , getMCChannel
       , getMCArray                  
       ) where

import Data.Hashable
import Control.Monad
import Data.Typeable
import Data.Either
import Data.List
import Data.Aeson 
import qualified Data.Aeson.Types as AET
import qualified Control.Monad.Error as ME
import qualified Data.HashMap as HM
import qualified Data.Vector as VC
import qualified Data.Text as DText

data SubscriberModelException = SubscriberModelException 
                              | IndexOutOfBoundsException
                              | UnknownKeyException
                              | ModelDoesNotMatchSpecSpecException
                              | InvalidModelSpecException
                              | NoModelException
                              | SubscriberAlreadyExistsException
                              | InvalidModelViewException
                              | NoSuchSubscriberException
                              | ModelClientException
                                deriving (Eq, Show)

instance ME.Error SubscriberModelException where
  noMsg = SubscriberModelException
  strMsg _ = SubscriberModelException

class (Show a) => ModelKey a where
  asKey :: a -> String
  
instance ModelKey String where asKey s = s
                               
instance ModelKey Char where asKey c = [c]

instance ModelKey DText.Text where asKey t = DText.unpack t

type StandardModel = Model String AET.Value
type StandardModelSpec = ModelSpec String
type StandardModelNode = ModelNode String AET.Value
type StandardModelClient = ModelClient String AET.Value
type StandardModelView' = ModelView' String

data Model a b = Model (ModelSpec a) (ModelNode a b) (HM.Map Int (Subscriber a))
               | NoModel
               deriving (Eq, Show)

data ModelSpec a = Req (MSN a)
                 | Opt (MSN a)
                 | NoModelSpec
                 deriving (Eq, Show)

data MSN a = SDN
           | SDCN Int
           | SDAN
           | MN (HM.Map a (ModelSpec a))
           | Wildcard (ModelSpec a)
           | MSNError
           | NoMSN
           deriving (Eq, Show)
                    
sdcnDefaultSize :: Int
sdcnDefaultSize = 20

sdnVP :: MSN a -> Bool
sdnVP SDN = True
sdnVP NoMSN = True
sdnVP _ = False

sdcnVP :: MSN a -> Maybe Int
sdcnVP (SDCN max') = Just max'
sdcnVP NoMSN = Just sdcnDefaultSize
sdcnVP _ = Nothing

sdanVP :: MSN a -> Bool
sdanVP SDAN = True
sdanVP NoMSN = True
sdanVP _ = False

mnVP :: (Hashable a, Ord a) => MSN a -> Maybe (a -> Maybe (ModelSpec a))
mnVP (MN m) = Just (`HM.lookup` m)
mnVP NoMSN = Just (\_ -> Just NoModelSpec)
mnVP _ = Nothing

data ModelNode a b = SDNode b
                   | SDChannelNode Int [b]
                   | SDArrayNode [b] 
                   | MapNode (HM.Map a (ModelNode a b))
                   | None
                   deriving (Eq, Show, Typeable)
                            
instance (Show a, ModelKey a, ToJSON b) => ToJSON (ModelNode a b) where
  toJSON (SDNode v) = object [ "type" .= ("node" :: String)
                             , "value" .= toJSON v ]
  toJSON (SDChannelNode m vs) = 
    object [ "type" .= ("channel" :: String)
           , "max" .= m
           , "value" .= VC.fromList (map (\e -> toJSON e) vs) ]
  toJSON (SDArrayNode vs) =
    object [ "type" .= ("array" :: String)
           , "value" .= VC.fromList (map (\e -> toJSON e) vs) ]
  toJSON (MapNode m) =
    object [ "type" .= ("map" :: String)
           , "value" .= 
             object (map (\(k, v) -> DText.pack (asKey k) .= (toJSON v)) $ 
                     HM.toList m) ]
  toJSON None = object []

data ModelNodeChange a b = SDNC b
                         | SDCNC b
                         | SDANC Int b
                         | SDANOC Int b
                         | SDANDC Int
                         | RKC a
                         | MNCError
                         deriving (Eq, Show)

instance (Show a, ToJSON b) => ToJSON (ModelNodeChange a b) where
  toJSON (SDNC v) = object [ "type" .= ("node" :: String)
                           , "value" .= toJSON v ]
  toJSON (SDCNC v) = object [ "type" .= ("channel" :: String)
                            , "value" .= toJSON v ]
  toJSON (SDANC i v) = object [ "type" .= ("arrayi" :: String)
                              , "index" .= i
                              , "value" .= toJSON v ]
  toJSON (SDANOC i v) = object [ "type" .= ("arrayo" :: String)
                               , "index" .= i
                               , "value" .= toJSON v ]
  toJSON (SDANDC i) =  object [ "type" .= ("arrayd" :: String)
                               , "index" .= i ]
  toJSON (RKC k) = object [ "type" .= ("mapd" :: String)
                          , "value" .= show k ]
  toJSON MNCError = object [ "type" .= ("error" :: String) ] 

data Subscriber a = Subscriber Int (ModelView a)
                deriving (Eq, Show, Typeable)
                         
data ModelView a = ModelView ModelViewVersion (ModelView' a)
                 deriving (Eq, Show)
                     
data ModelView' a = ViewNode (HM.Map a (ModelView' a))
                  | ViewLeaf
                  deriving (Eq, Show)
                          
type ModelViewVersion = Int
                     
data ModelUpdateMessage a b = 
  ModelUpdateMessage Int ModelViewVersion [a] (ModelNodeChange a b)
  | ModelResetMessage Int ModelViewVersion (ModelNode a b)
  deriving (Eq, Show, Typeable)

instance (Show a, ModelKey a, ToJSON b) => ToJSON (ModelUpdateMessage a b) where
  toJSON (ModelUpdateMessage i v path mnc) = 
    object [ "type" .= ("ModelUpdateMessage" :: String)
           , "uid" .= i 
           , "version" .= v
           , "path" .= VC.fromList (map (\e -> asKey e) path)
           , "node-change" .= toJSON mnc ]
  toJSON (ModelResetMessage i v mn) =
    object [ "type" .= ("ModelResetMessage" :: String)
           , "uid" .= i
           , "version" .= v
           , "value" .= toJSON mn ]

data ModelUpdateMessage' = ModelUnsubscribedMessage Int
                           deriving (Eq, Show, Typeable)

instance ToJSON ModelUpdateMessage' where
  toJSON _ = undefined

msgVerMax :: Int
msgVerMax = 10000

createModel :: (Hashable a, Ord a) =>
               ModelSpec a -> ModelNode a b -> 
               Either SubscriberModelException (Model a b)
createModel ms mn = 
  let changesAndPaths = toPath [] mn
      mn' = foldM (\m (p, mnc) -> updateModel' ms m p mnc) 
            None changesAndPaths `ME.catchError` Left
  in case mn' of 
    Right mn'' -> if verifyModel ms mn''
                  then Right $ Model ms mn'' HM.empty
                  else Left ModelDoesNotMatchSpecSpecException
    Left _ -> Left ModelDoesNotMatchSpecSpecException

toPath :: [a] -> ModelNode a b -> [([a], ModelNodeChange a b)]
toPath path (SDNode sd) = [(path, SDNC sd)]
toPath path (SDChannelNode i ss)
      | i < length ss = [([], MNCError)]
      | otherwise =  map (\s -> (path, SDCNC s))  (reverse ss)
toPath path (SDArrayNode ss) = 
  fst (foldl (\(ss', i) s -> ((path, SDANC i s):ss', i-1)) 
       ([], length ss - 1) (reverse ss))
toPath path (MapNode m) =
  HM.foldWithKey (\k v ss ->
                   ss++
                   map (\(p, mnc) -> (p, mnc))
                   (toPath (path ++ [k]) v)) [] m
toPath _ None = error "cannot match path to non-existent ModelNode"

verifyModel :: (Hashable a, Ord a) =>
               ModelSpec a -> ModelNode a b -> Bool
verifyModel (Req ms) mn = verifyModel' ms mn
verifyModel (Opt ms) mn = verifyModel' ms mn 
verifyModel NoModelSpec _ = True

verifyModel' :: (Hashable a, Ord a) => 
               MSN a -> ModelNode a b -> Bool
verifyModel' SDN (SDNode _) = True
verifyModel' (SDCN _) (SDChannelNode _ _) = True
verifyModel' SDAN (SDArrayNode _) = True
verifyModel' (MN m) (MapNode m') = 
  HM.foldWithKey (\k v ok -> ok && case HM.lookup k m' of
                     Just v' -> verifyModel v v'
                     Nothing -> case v of 
                       Req _ -> False
                       Opt _ -> True
                       NoModelSpec -> True) True m
verifyModel' (Wildcard m) (MapNode m') = 
  HM.foldWithKey (\_ v ok -> ok && verifyModel m v) True m'
verifyModel' _ _ = False

createSubscriber :: Int -> ModelView' a -> Subscriber a
createSubscriber i mv = Subscriber i $ ModelView 1 mv

addSubscriber :: (Show a, Hashable a, Ord a) =>
                 Model a b -> Subscriber a -> 
                 Either SubscriberModelException (Model a b)
addSubscriber m@(Model spec nodes subscribers) 
  s@(Subscriber i (ModelView _ v')) = 
  if not $ isSubscriber m i
  then if verifyView spec v'
       then Right $ Model spec nodes $ HM.insert i s subscribers
       else Left InvalidModelViewException
  else Left SubscriberAlreadyExistsException
addSubscriber NoModel _ = Left NoModelException

removeSubscriber :: (Show a, Hashable a, Ord a) =>
                    Model a b -> Int -> 
                    Either SubscriberModelException (Model a b)
removeSubscriber (Model spec nodes subscribers) i =
  if HM.member i subscribers
  then Right $Model spec nodes $ HM.delete i subscribers
  else Left NoSuchSubscriberException
removeSubscriber NoModel _ = Left NoModelException

isSubscriber :: (Eq a) => Model a b -> Int -> Bool
isSubscriber (Model _ _ subscribers) i = HM.member i subscribers
isSubscriber NoModel _ = False

verifyView :: (Show a, Hashable a, Ord a) =>
              ModelSpec a -> ModelView' a -> Bool
verifyView _ ViewLeaf = True
verifyView (Opt (MN m)) (ViewNode m') = verifyView'' m m'
verifyView (Req (MN m)) (ViewNode m') = verifyView'' m m'
verifyView (Opt (Wildcard ms)) (ViewNode m) = verifyView' ms m
verifyView (Req (Wildcard ms)) (ViewNode m) = verifyView' ms m
verifyView _ _ = False

verifyView' :: (Hashable a, Show a, Ord a) =>
               ModelSpec a -> HM.Map t (ModelView' a) -> Bool
verifyView' ms m = all (\(_, v) -> verifyView ms v) $ HM.assocs m
  
verifyView'' :: (Hashable a, Show a, Ord a) =>
                HM.Map a (ModelSpec a) -> HM.Map a (ModelView' a) -> Bool
verifyView'' m m' = all (\(k, v) -> case HM.lookup k m  of
                            Just v' -> verifyView v' v
                            Nothing -> False) $ HM.assocs m'

getSubscribers :: (Hashable a, Ord a) =>
                  Model a b -> [a] -> [Subscriber a]
getSubscribers (Model _ _ subscribers) p =
  HM.fold (\s@(Subscriber _ (ModelView _ v)) subs -> 
            if viewMatchesPath p v
            then s:subs
            else subs) [] subscribers
getSubscribers NoModel _ = []

viewMatchesPath :: (Hashable a, Ord a) => [a] -> ModelView' a -> Bool
viewMatchesPath [] ViewLeaf = True 
viewMatchesPath (a:as) (ViewNode m) = 
  case HM.lookup a m of
    Just v -> viewMatchesPath as v
    Nothing -> False
viewMatchesPath _ _ = False

resetModel :: (Show a, Hashable a, Ord a) =>
              Model a b -> ModelNode a b ->
              Either SubscriberModelException 
              (Model a b, [ModelUpdateMessage a b])
resetModel (Model s n oldsubs) n' = 
  if verifyModel s n'
  then let subs = HM.elems oldsubs
           updates = map (\(Subscriber k (ModelView ver mv')) ->
                           let nodes = modelNodesForModelView n mv'
                           in case nodes of
                             Right ns -> Right $ ModelResetMessage k (ver + 1) 
                                         ns
                             Left e -> Left e) subs
           newsubs = map (\(Subscriber k (ModelView ver mv')) -> 
                           (k, Subscriber k 
                               (ModelView ((ver + 1) `mod` msgVerMax) 
                                mv'))) subs
           newsubs' = HM.union (HM.fromList newsubs) oldsubs 
       in if null (lefts updates)
          then Right (Model s n' newsubs', rights updates)
          else Left ModelDoesNotMatchSpecSpecException
  else Left ModelDoesNotMatchSpecSpecException
resetModel NoModel _ = Left NoModelException

modelNodesForModelView :: (ME.MonadError SubscriberModelException m, 
                           Show a, Hashable a, Ord a) =>
                          ModelNode a b -> ModelView' a -> m (ModelNode a b)
modelNodesForModelView n ViewLeaf = return n
modelNodesForModelView (MapNode nm) (ViewNode vm) = do
  let mapFun k mn = case HM.lookup k vm of
        Just vn -> modelNodesForModelView mn vn `ME.catchError` Left
        Nothing ->
          -- can never occur because of intersection
          Left InvalidModelViewException
      (lts, rts) = HM.mapEitherWithKey mapFun (HM.intersection nm vm)
  if HM.size lts == 0
    then return $ MapNode rts
    else ME.throwError InvalidModelViewException
modelNodesForModelView _ _ = 
  -- can never occur because ModelView has been verified for this model
  ME.throwError InvalidModelViewException

updateModel :: (Show a, Hashable a, Ord a) => 
            Model a b -> [a] -> ModelNodeChange a b -> 
            Either SubscriberModelException 
            (Model a b, [ModelUpdateMessage a b])
updateModel m@(Model s n oldsubs) p mnc = case updateModel' s n p mnc of
  Right m' -> let subs = getSubscribers m p
                  updates = map (\(Subscriber k (ModelView ver _)) -> 
                                  ModelUpdateMessage k (ver + 1) p mnc) subs
                  newsubs = map (\(Subscriber k (ModelView ver mv')) -> 
                                  (k, Subscriber k 
                                      (ModelView ((ver + 1) `mod` msgVerMax) 
                                       mv'))) subs
                  newsubs' = HM.union (HM.fromList newsubs) oldsubs 
              in Right (Model s m' newsubs', updates)
  Left e -> Left e
updateModel NoModel _ _ = Left NoModelException

updateModel' :: (ME.MonadError SubscriberModelException m, 
                Hashable a, Ord a) =>
               ModelSpec a -> ModelNode a b -> [a] -> 
               ModelNodeChange a b ->
               m (ModelNode a b)
updateModel' (Req ms) mn ps mnc = updateModel'' ms mn ps mnc
updateModel' (Opt ms) mn ps mnc = updateModel'' ms mn ps mnc
updateModel' NoModelSpec mn ps mnc = updateModel'' NoMSN mn ps mnc
  
updateModel'' :: (ME.MonadError SubscriberModelException m,
                Hashable a, Ord a) =>
                MSN a -> ModelNode a b -> [a] -> ModelNodeChange a b ->
                m (ModelNode a b)
                
updateModel'' (sdnVP -> True) (SDNode _) [] (SDNC sdn') = return $ SDNode sdn'
updateModel'' (sdnVP -> True) None [] (SDNC sdn') = return $ SDNode sdn'

updateModel'' (sdcnVP -> Just max') (SDChannelNode num sds) [] (SDCNC sd)
  | num >= (max' * 2) - 1 = 
    return $ SDChannelNode max' (sd:take (max' - 1) sds)
  | otherwise = return $ SDChannelNode (num + 1) (sd:sds)
updateModel'' (sdcnVP -> Just _) None [] (SDCNC sd) = return $ 
                                                         SDChannelNode 1 [sd]

updateModel'' (sdanVP -> True) (SDArrayNode sds) [] (SDANC i sd)
  | (i < 0) || (i > length sds) = ME.throwError IndexOutOfBoundsException
  | otherwise = return (SDArrayNode $ let (ys,zs) = splitAt i sds   
                                      in ys ++ [sd] ++ zs)
updateModel'' (sdanVP -> True) None [] (SDANC 0 sd) = return $ SDArrayNode [sd]
updateModel'' (sdanVP -> True) (SDArrayNode sds) [] (SDANOC i sd)
  | (i < 0) || (i >= length sds) = ME.throwError IndexOutOfBoundsException
  | otherwise = return (SDArrayNode $ let (ys,_:zs) = splitAt i sds   
                                      in ys ++ [sd] ++ zs)
updateModel'' (sdanVP -> True) (SDArrayNode sds) [] (SDANDC i)
  | (i < 0) || (i >= length sds) = ME.throwError IndexOutOfBoundsException
  | otherwise = return (SDArrayNode $ let (ys,_:zs) = splitAt i sds   
                                      in ys ++ zs)

updateModel'' (mnVP -> Just lkup) (MapNode mm) (sk:ps) nc = 
  case lkup sk of
    Just msNext -> case HM.lookup sk mm of
      Just mnNext -> do
        um <- updateModel' msNext mnNext ps nc
        return $ MapNode $ HM.insert sk um mm
      Nothing -> do
        um <- updateModel' msNext None ps nc
        return $ MapNode $ HM.insert sk um mm
    Nothing -> ME.throwError UnknownKeyException
updateModel'' (mnVP -> Just lkup) None (sk:ps) nc = 
  case lkup sk of
    Just msNext -> do
      um <- updateModel' msNext None ps nc
      return $ MapNode $ HM.singleton sk um
    Nothing -> ME.throwError UnknownKeyException
updateModel'' (mnVP -> Just lkup) (MapNode mm) [] (RKC sk) =
  case lkup sk of
    Just _ -> case HM.lookup sk mm of
      Just _ -> return $ MapNode $ HM.delete sk mm
      Nothing -> ME.throwError UnknownKeyException
    Nothing -> ME.throwError UnknownKeyException

updateModel'' (Wildcard msNext) (MapNode mm) (sk:ps) nc = 
  case HM.lookup sk mm of
    Just mnNext -> do
      um <- updateModel' msNext mnNext ps nc
      return $ MapNode $ HM.insert sk um mm
    Nothing -> do
      um <- updateModel' msNext None ps nc
      return $ MapNode $ HM.insert sk um mm
updateModel'' (Wildcard msNext) None (sk:ps) nc = do
  um <- updateModel' msNext None ps nc
  return $ MapNode $ HM.singleton sk um

updateModel'' (MSNError) _ _ _ = ME.throwError InvalidModelSpecException

updateModel'' _ _ _ _ = ME.throwError InvalidModelSpecException

getMNode :: (Hashable a, Ord a) =>
              Model a b -> [a] -> Maybe b
getMNode (Model _ mn _) path =
  case get mn path of 
    Just (SDNode v) -> Just v
    _ -> Nothing
getMNode NoModel _ = Nothing
    
getMChannel :: (Hashable a, Ord a) =>
                Model a b -> [a] -> Maybe [b]
getMChannel (Model _ mn _) path =
  case get mn path of 
    Just (SDChannelNode _ v) -> Just v
    _ -> Nothing
getMChannel NoModel _ = Nothing
    
getMArray :: (Hashable a, Ord a) =>
              Model a b -> [a] -> Maybe [b]
getMArray (Model _ mn _) path =
  case get mn path of 
    Just (SDArrayNode v) -> Just v
    _ -> Nothing
getMArray NoModel _ = Nothing

-- model spec

wildcard :: ModelSpec a -> MSN a
wildcard = Wildcard

dict :: (Hashable a, Ord a) => [(a, ModelSpec a)] -> ModelSpec a
dict kvs = Opt $ MN (HM.fromList kvs)

rdict :: (Hashable a, Ord a) => [(a, ModelSpec a)] -> ModelSpec a
rdict kvs = Req $ MN (HM.fromList kvs)

req :: MSN a -> ModelSpec a
req = Req

opt :: MSN a -> ModelSpec a
opt = Opt

node :: MSN a
node = SDN

channel :: Int -> MSN a
channel max' = if max' >= 1
            then SDCN max'
            else MSNError

array :: MSN a
array = SDAN

-- model

node' :: b -> ModelNode a b
node' = SDNode 

channel' :: [b] -> ModelNode a b
channel' cs = SDChannelNode (length cs) cs

array' :: [b] -> ModelNode a b
array' = SDArrayNode

dict' :: (Hashable a, Ord a) => [(a, ModelNode a b)] -> ModelNode a b
dict' kvs = MapNode $ HM.fromList kvs

-- updates

nnode :: b -> ModelNodeChange a b
nnode = SDNC

arrayi :: Int -> b -> ModelNodeChange a b
arrayi = SDANC

arrayo :: Int -> b -> ModelNodeChange a b
arrayo = SDANOC

arrayd :: Int -> ModelNodeChange a b
arrayd = SDANDC

channeli :: b -> ModelNodeChange a b
channeli = SDCNC

rmkey :: a -> ModelNodeChange a b
rmkey = RKC

-- view

vmap :: (Hashable a, Ord a) => [(a, ModelView' a)] -> ModelView' a
vmap [] = ViewLeaf
vmap vs = ViewNode $ HM.fromList vs 

vleaf :: ModelView' a
vleaf = ViewLeaf

-- model client

data ModelClient a b = ModelClient Int [ModelUpdateMessage a b] 
                       (ModelNode a b)
                     | NoModelClient
                     deriving (Eq, Show)

createModelClient :: ModelClient a b
createModelClient = NoModelClient

getModel :: ModelClient a b -> Maybe (ModelNode a b)
getModel (ModelClient _ _ model) = Just model
getModel _ = Nothing

sortMUM :: ModelUpdateMessage a b -> ModelUpdateMessage a b -> Ordering
sortMUM (ModelUpdateMessage _ i _ _) (ModelUpdateMessage _ j _ _) = compare i j
sortMUM (ModelUpdateMessage _ i _ _) (ModelResetMessage _ j _) = compare i j
sortMUM (ModelResetMessage _ i _) (ModelUpdateMessage _ j _ _) = compare i j
sortMUM (ModelResetMessage _ i _) (ModelResetMessage _ j _) = compare i j

getMCNode :: (Hashable a, Ord a) =>
              ModelClient a b -> [a] -> Maybe b
getMCNode (ModelClient _ _ mn) path =
  case get mn path of 
    Just (SDNode v) -> Just v
    _ -> Nothing
getMCNode NoModelClient _ = Nothing
    
getMCChannel :: (Hashable a, Ord a) =>
                ModelClient a b -> [a] -> Maybe [b]
getMCChannel (ModelClient _ _ mn) path =
  case get mn path of 
    Just (SDChannelNode _ v) -> Just v
    _ -> Nothing
getMCChannel NoModelClient _ = Nothing
    
getMCArray :: (Hashable a, Ord a) =>
              ModelClient a b -> [a] -> Maybe [b]
getMCArray (ModelClient _ _ mn) path =
  case get mn path of 
    Just (SDArrayNode v) -> Just v
    _ -> Nothing
getMCArray NoModelClient _ = Nothing
    
get :: (Hashable k, Ord k) =>
       ModelNode k t -> [k] -> Maybe (ModelNode k t)
get n@(SDNode _) [] = Just n    
get n@(SDChannelNode _ _) [] = Just n
get n@(SDArrayNode _) [] = Just n
get (MapNode hm) (p:ps) =
  case HM.lookup p hm of
    Just nn -> get nn ps
    Nothing -> Nothing
get _ _ = Nothing

handleModelUnsubscribedMessage :: ModelClient a b -> ModelClient a b
handleModelUnsubscribedMessage _ = NoModelClient

handleModelUpdateMessage :: 
    (Hashable a, Ord a, Show a) =>
    ModelClient a b -> ModelUpdateMessage a b ->
    Either SubscriberModelException (ModelClient a b)
handleModelUpdateMessage (ModelClient version mqueue mn) mum =
  let mqueue' = sortBy sortMUM (mum:mqueue)
      result = processAllQueuedMessages version mqueue' mn
  in case result of
    Right (version', mqueue'', mn') ->
      Right $ ModelClient version' mqueue'' mn'
    Left e -> Left e
handleModelUpdateMessage NoModelClient (ModelResetMessage _ v mn) =
  Right $ ModelClient v [] mn
handleModelUpdateMessage NoModelClient _ = Left ModelClientException

processAllQueuedMessages :: 
  (Hashable a, Ord a, Show a) =>
  ModelViewVersion -> [ModelUpdateMessage a b] -> ModelNode a b ->
  Either SubscriberModelException (ModelViewVersion, [ModelUpdateMessage a b],
                                   ModelNode a b)
processAllQueuedMessages version [] mn = Right (version, [], mn)
processAllQueuedMessages version (m:msgs) mn =
  if getVersion m == version + 1
  then case m of 
    (ModelUpdateMessage _ version' path mnc) -> 
      case updateModel' NoModelSpec mn path mnc of 
        Right mn' -> processAllQueuedMessages version' msgs mn'
        Left e -> Left e
    (ModelResetMessage _ version' mn') -> 
      processAllQueuedMessages version' msgs mn'
  else Right (version, m:msgs, mn)
  where getVersion (ModelUpdateMessage _ v _ _) = v
        getVersion (ModelResetMessage _ v _) = v


  
  

