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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module EventSourcing
  ( EventSourcing
  , EventPersistorConstraint
  , execute
  , newEventSourcing
  )
  where

import EventSourcing.Aggregate (Aggregate, Command, Event, Error, handle, apply)
import EventSourcing.Spi

data EventSourcing p s where
  EventSourcingImpl :: (EventPersistor p, StateManager s) => p -> s -> EventSourcing p s

newEventSourcing :: (EventPersistor p, StateManager s) => p -> s -> EventSourcing p s
newEventSourcing = EventSourcingImpl

execute :: (Aggregate b, EventPersistorConstraint p (Event b)) => EventSourcing p s -> Command b -> IO (Either (Error b) (Event b))
execute (EventSourcingImpl persistor stateManager) cmd =
  currentState stateManager >>= updatedState >>= executeState
  where

    executeState maybeStateEnvelope =
      let maybePrevState = fmap statePayload maybeStateEnvelope
          maybePrevEventId = fmap lastEventId maybeStateEnvelope
      in
        case handle cmd maybePrevState of
          err@(Left _) -> return err
          correct@(Right ev) -> do
            result <- writeEvent persistor maybePrevEventId ev
            case result of
              Ok eventEnvelope -> saveState stateManager (eventId eventEnvelope) (apply maybePrevState ev) >> return correct
              Rejected -> updatedState maybeStateEnvelope >>= executeState

    updatedState maybeStateEnvelope = do
      let prevEventId = fmap lastEventId maybeStateEnvelope
      events <- loadEvents persistor prevEventId
      let result = foldl applyEvent maybeStateEnvelope events
      return result
      where
        applyEvent maybeStateEnvelope2 envelope = Just $ StateEnvelope (eventId envelope) (apply (fmap statePayload maybeStateEnvelope2) (eventPayload envelope))
