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

module EventSourcing.DummyStateManager
    ( DummyStateManager
    , newDummyStateManager
    ) where

import EventSourcing.Spi (StateManager, StateEnvelope(StateEnvelope), currentState, saveState)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Typeable (TypeRep, typeOf)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import qualified Data.Map.Strict as Map

newDummyStateManager :: DummyStateManager

data DummyStateManager = DummyStateManagerImpl
  deriving (Show)

instance StateManager DummyStateManager where

  currentState _ = do
    putStrLn "currentState called"
    return Nothing

  saveState _ eventId newState = do
    putStrLn $ "saveState eventId: " ++ show eventId ++ " newState: " ++ show newState
    return ()

newDummyStateManager = DummyStateManagerImpl
