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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module EventSourcing.Spi
    ( EventResult(Ok, Rejected)
    , EventEnvelope(EventEnvelope, eventId, timestamp, previousEventId, eventPayload)
    , StateEnvelope(StateEnvelope, statePayload, lastEventId)
    , EventPersistorConstraint
    , EventPersistor
    , StateManager
    , writeEvent
    , loadEvents
    , currentState
    , saveState
    ) where

import EventSourcing.Aggregate (Aggregate, Event)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Typeable (Typeable)
import GHC.Exts (Constraint)

data EventEnvelope b = EventEnvelope { eventId :: UUID, timestamp :: UTCTime, previousEventId :: Maybe UUID, eventPayload :: Event b }
  deriving (Typeable)

instance Show (EventEnvelope b) where
  show ev = "{eventId: " ++ show (eventId ev) ++ ", timestamp: " ++ show (timestamp ev) ++ "}"

type family EventPersistorConstraint a b :: Constraint

data StateEnvelope b = StateEnvelope { lastEventId :: UUID, statePayload :: b}

data EventResult b = Ok (EventEnvelope b)
                 | Rejected

class EventPersistor a where
 writeEvent :: (Aggregate b, EventPersistorConstraint a (Event b) ) => a -> Maybe UUID -> Event b -> IO (EventResult b)
 loadEvents :: (Aggregate b, EventPersistorConstraint a (Event b) ) => a -> Maybe UUID -> IO [EventEnvelope b]

class StateManager a where
 currentState :: (Aggregate b) => a -> IO (Maybe (StateEnvelope b))
 saveState :: (Aggregate b) => a -> UUID -> b -> IO ()
