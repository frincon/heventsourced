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
module EventSourcing.InMemoryEventPersistor
    ( InMemoryEventPersistor
    , newInMemoryEventPersistor
    ) where

import Development.Placeholders
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import EventSourcing.Spi (EventPersistor, EventResult(Ok, Rejected), EventEnvelope(EventEnvelope, eventId, eventPayload), writeEvent, loadEvents)
import Data.Typeable (Typeable, TypeRep, Proxy(Proxy), typeRep)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Time (UTCTime, getCurrentTime)
import EventSourcing.Aggregate (Aggregate, Event)

newInMemoryEventPersistor :: IO InMemoryEventPersistor

data InMemoryEventPersistor = InMemoryEventPersistorImpl (TVar (Map.Map TypeRep [(UUID, Dynamic)]))

instance EventPersistor InMemoryEventPersistor where
  writeEvent (InMemoryEventPersistorImpl p) lastEventId newEvent = do
    newId <- nextRandom
    newTimeStamp <- getCurrentTime
    atomically $ compareAndModify newId newTimeStamp where

    proxyFromEvent :: Typeable b1 => t b1 -> Proxy b1
    proxyFromEvent _ = Proxy

    compareAndModify newId newTimeStamp = do
      currentEventId <- getLastEvent (typeRep $ proxyFromEvent newEvent)
      if currentEventId == lastEventId
        then do
          let newEventEnvelope = EventEnvelope newId newTimeStamp lastEventId newEvent
          storeEvent (typeRep $ proxyFromEvent newEvent) newEventEnvelope
          return $ Ok newEventEnvelope
        else
          return Rejected


    getLastEvent aggType = do
      mapValue <- readTVar p
      return $ Map.lookup aggType mapValue >>= lastUUID
      where
        lastUUID :: [(UUID, Dynamic)] -> Maybe UUID
        lastUUID [] = Nothing
        lastUUID list = Just uuid where
          (uuid, _) = last list

    storeEvent aggType eventEnvelope =
      modifyTVar p (addEvent aggType eventEnvelope)
      where
        -- addEvent :: TypeRep -> EventEnvelope b -> Map.Map TypeRep [(UUID, Dynamic)] -> Map.Map TypeRep [(UUID, Dynamic)]
        addEvent aggType eventEnvelope mapValue =
          Map.insert aggType (fromMaybe [] (Map.lookup aggType mapValue) ++ [(eventId eventEnvelope, toDyn eventEnvelope)]) mapValue


  loadEvents =
    loadEvents' Proxy where

      loadEvents' :: Aggregate b => Proxy b -> InMemoryEventPersistor -> Maybe UUID -> IO [EventEnvelope b]
      loadEvents' proxy (InMemoryEventPersistorImpl p) fromEventId = do
        mapValue <- atomically $ readTVar p
        let maybeList = Map.lookup (typeRep proxy) mapValue
        let resultList = fromMaybe [] maybeList
        let secondList = map snd resultList
        return $ mapMaybe fromDynamic secondList

newInMemoryEventPersistor = atomically $ fmap InMemoryEventPersistorImpl (newTVar Map.empty)
