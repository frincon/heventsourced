-- Copyright (c) 2016 Fernando Rincon
--
-- Based on the publication https://gist.github.com/Fristi/7327904
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AutoDeriveTypeable #-}
module EventSourcing.Aggregate
  ( Aggregate
  , Command
  , Event
  , Error
  , handle
  , apply
  )
  where

import Data.Typeable

class (Show (Command s), Show (Event s), Show (Error s), Typeable s, Show s) => Aggregate s where
  data Command s
  data Event s
  data Error s

  handle :: Command s -> Maybe s -> Either (Error s) (Event s)
  apply :: Maybe s -> Event s -> s
