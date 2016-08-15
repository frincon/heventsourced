-- Copyright (c) 2016 Fernando Rincon
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module EventSourcing.Mongo.MongoEventPersistor
    ( newMongoEventPersistor
    , MongoEventPersistor
    , ToBSON(toBSON)
    , FromBSON(fromBSON)
    ) where

import Data.UUID (UUID, fromText)
import Data.UUID.V4 (nextRandom)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Data.Dynamic (Typeable, TypeRep, Proxy(Proxy), typeRep)
import EventSourcing.Spi (EventPersistor(writeEvent, loadEvents), EventEnvelope(EventEnvelope, eventId, previousEventId, timestamp, eventPayload), EventResult(Ok, Rejected), EventPersistorConstraint)
import EventSourcing.Aggregate (Aggregate, Event)
import Data.Text (pack)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Exception.Base (catch)
import Database.MongoDB
import Control.Monad.Reader
import Development.Placeholders

newMongoEventPersistor :: Pipe ->  MongoEventPersistor

class FromBSON a where
  fromBSON :: Document -> Maybe a

class ToBSON a where
  toBSON :: a -> Document

data MongoEventPersistor = MongoEventPersistorImpl Pipe

type instance EventPersistorConstraint MongoEventPersistor b = (FromBSON b, ToBSON b)

newMongoEventPersistor = MongoEventPersistorImpl

instance EventPersistor MongoEventPersistor where

  writeEvent (MongoEventPersistorImpl pipe) lastEventId newEvent = do
    newId <- nextRandom
    newTimeStamp <- getCurrentTime
    sequenceNumber <- getNextSequenceNumber $ typeRepFromEvent newEvent
    access pipe master "test" $ ensureIndexes $ typeRepFromEvent newEvent
    catch (access pipe master "test" $ compareAndModify sequenceNumber newId newTimeStamp) writeFailureToRejected
    where

      getNextSequenceNumber :: TypeRep -> IO Integer
      getNextSequenceNumber aggName = do
        result <- access pipe master "test" $ findAndModifyOpts (select ["_id" =: pack (show aggName)] "counter") (FamUpdate [ "$inc" =: ["seq" =: (1 :: Integer)]] True True)
        case result of
          Right (Just document) -> Database.MongoDB.lookup "seq" document
          _ -> error "Error trying to get next sequence number"

      writeFailureToRejected (WriteFailure _ errorString) = do
        putStrLn errorString
        return Rejected

      indexesNeeded =
        [(pack "previousEventId_1", ["previousEventId" =: (1 :: Integer)])]

      ensureIndexes aggType =
        foldl ensure (return ()) indexesNeeded
        where
          ensure io doc = io >> ensureIndex Index {iColl = pack $ show aggType, iKey = snd doc, iName = fst doc, iUnique = True, iDropDups = False, iExpireAfterSeconds = Nothing }

      typeRepFromEvent :: Typeable b1 => t1 b1 -> TypeRep
      typeRepFromEvent = typeRep . proxyFromEvent

      proxyFromEvent :: Typeable b1 => t b1 -> Proxy b1
      proxyFromEvent _ = Proxy

      compareAndModify sequenceNumber newId newTimeStamp = do
        let newEventEnvelope = EventEnvelope newId newTimeStamp lastEventId newEvent
        let aggName = pack $ show $ typeRepFromEvent newEvent
        newEventId <- saveNewEvent aggName sequenceNumber newEventEnvelope
        updatedResult <- updateEvent aggName lastEventId newId
        case updatedResult of
          (Right maybeDoc) -> return $ Ok newEventEnvelope
          _ -> do
            removeEvent aggName newEvent
            return Rejected

      saveNewEvent aggName sequenceNumber newEventEnvelope = do
        save aggName document
        liftIO $ putStrLn $ "Inserted" ++ show document
        where
          document = [
                        "eventId" =: show (eventId newEventEnvelope),
                        "timestamp" =: EventSourcing.Spi.timestamp newEventEnvelope,
                        "nextEventId" =: Null,
                        "previousEventId" =: case previousEventId newEventEnvelope of Nothing -> Null
                                                                                      Just prevId -> String (pack $ show prevId),
                        "sequenceNumber" =: sequenceNumber,
                        "eventPayload" =: toBSON newEvent
                     ]


      updateEvent aggName Nothing newId = return $ Right Nothing

      updateEvent aggName (Just lastId) newId =
        findAndModifyOpts (select queryDoc aggName) (FamUpdate updateDoc False False)
        where
          queryDoc = ["eventId" =: show lastId, "nextEventId" =: Null]
          updateDoc = ["$set" =: ["nextEventId" =: show newId]]

      removeEvent aggName eventId =
        delete (select ["eventId" =: show eventId] aggName)

  loadEvents (MongoEventPersistorImpl pipe)  =
    access pipe master "test" . loadEvents' Proxy  where

      loadEvents' :: (Aggregate b, EventPersistorConstraint MongoEventPersistor (Event b)) => Proxy b -> Maybe Data.UUID.UUID -> Action IO [EventEnvelope b]
      loadEvents' proxy Nothing = loadEventsWithQuery proxy []
      loadEvents' proxy (Just fromEventId) = do
        seqNumber <- sequenceNumberOfEventId
        loadEventsWithQuery proxy ["sequenceNumber" =: ["$gt" =: seqNumber]]
        where
          sequenceNumberOfEventId :: Action IO Integer
          sequenceNumberOfEventId = do
            maybeEventDocument <- findOne $ select ["eventId" =: pack (show fromEventId)] (pack $ show $ typeRep proxy)
            case maybeEventDocument of
              (Just eventDocument) -> return $ typed $ valueAt "sequenceNumber" eventDocument
              _ -> error $ "Cannot find event with id " ++ show fromEventId

      loadEventsWithQuery :: (Aggregate b, EventPersistorConstraint MongoEventPersistor (Event b)) => Proxy b -> Document -> Action IO [EventEnvelope b]
      loadEventsWithQuery proxy query = do
        queryResult <- find $ (select query (pack $ show $ typeRep proxy)) {sort = ["sequenceNumber" =: (1 :: Integer)]}
        documents <- rest queryResult
        return $ map toEventEnvelope documents
        where
          toEventEnvelope :: (Aggregate b, EventPersistorConstraint MongoEventPersistor (Event b)) => Document -> EventEnvelope b
          toEventEnvelope document =  EventEnvelope (getUUID "eventId" document) (getUTCTime "timestamp" document) (lookupUUID "previousEventId" document) (fromJust $ fromBSON $ getDocument "eventPayload" document) where
            getUUID label = fromJust . fromText . typed . valueAt label
            getUTCTime label = typed . valueAt label
            lookupUUID label document =  document !? label >>= fromText
            getDocument label document = let Doc subdocument = valueAt label document in
              subdocument
