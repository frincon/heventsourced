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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Erp.World
  ( World(..)
  , Command(..)
  , Event(..)
  , Error(..)
  )
  where

import Erp.Commons
import EventSourcing.Aggregate
import Data.Data
import Data.Maybe (fromJust)
import EventSourcing.Mongo (ToBSON(toBSON), FromBSON(fromBSON))
import Database.MongoDB hiding (Command)
import Data.Text (pack)
import Development.Placeholders

data FiscalPerson = FiscalPerson Name Address deriving (Show, Data)
data NaturalPerson = NaturalPerson Name Address deriving (Show, Data)

data World = WorldImpl {
  name :: Name,
  naturalPersonList :: [NaturalPerson],
  fiscalPersonList :: [FiscalPerson]
} deriving (Show, Data)

instance Aggregate World where
  data Command World =
      RegisterNaturalPerson Name Address
    | RegisterFiscalPerson Name Address
    | Initialize Name
    deriving (Show)

  data Event World  =
      NaturalPersonRegistered NaturalPerson
    | FiscalPersonRegistered FiscalPerson
    | Initialized Name
    deriving (Show)

  data Error World =
      NaturalPersonAlreadyRegistered
    | FiscalPersonAlreadyRegistered
    | NotInitialized
    | AlreadyInitialized
    deriving (Show)

  handle (RegisterNaturalPerson theName address) (Just _) = Right $ NaturalPersonRegistered $ NaturalPerson theName address
  handle (RegisterFiscalPerson theName address) (Just _) = Right $ FiscalPersonRegistered $ FiscalPerson theName address
  handle (Initialize theName) Nothing = Right $ Initialized theName
  handle _ Nothing = Left NotInitialized
  handle _ (Just _) = Left AlreadyInitialized

  apply (Just state) (NaturalPersonRegistered naturalPerson) = state { naturalPersonList = naturalPerson : naturalPersonList state}
  apply (Just state) (FiscalPersonRegistered fiscalPerson) = state { fiscalPersonList = fiscalPerson : fiscalPersonList state}
  apply Nothing (Initialized initName) = WorldImpl { name = initName, naturalPersonList = [], fiscalPersonList = []}
  apply _ _ = error "Cannot be possible"

instance ToBSON (Event World) where
  toBSON (NaturalPersonRegistered (NaturalPerson (Name theName) (Address address))) = ["eventType" =: pack "NaturalPersonRegistered", "data" =: ["name" =: theName, "address" =: address]]
  toBSON (FiscalPersonRegistered (FiscalPerson (Name theName) (Address address))) = ["eventType" =: pack "FiscalPersonRegistered", "data" =: ["name" =: theName, "address" =: address]]
  toBSON (Initialized (Name theName)) = ["eventType" =: pack "Initialize", "data" =: ["name" =: theName]]

instance FromBSON (Event World) where
  fromBSON document = case valueAt "eventType" document of
    String "NaturalPersonRegistered" -> Just $ NaturalPersonRegistered $ NaturalPerson (Name $ fromJust $ document !? "data.name") (Address $ fromJust $ document !? "data.address")
    String "FiscalPersonRegistered" -> Just $ FiscalPersonRegistered $ FiscalPerson (Name $ fromJust $ document !? "data.name") (Address $ fromJust $ document !? "data.address")
    String "Initialize" -> Just $ Initialized $ Name $ fromJust $ document !? "data.name"
    _ -> Nothing
