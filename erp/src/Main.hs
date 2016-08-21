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
module Main where

import Data.Maybe (fromMaybe)
import Data.List.Split
import Control.Monad
import Erp.Commons (Name(Name), Address(Address))
import EventSourcing
import EventSourcing.DummyStateManager
import EventSourcing.Mongo.MongoEventPersistor
import Database.MongoDB (Pipe, connect, close, PortID(PortNumber), defaultPort, Host(Host))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import CommandLineParser (parseCommandLine)
import qualified CmdOptions as Opts
import qualified Erp.World as World

data Command = Exit |
               WorldCommand (World.Command World.World)
               deriving (Show)

data CommandResult p s = CommandResult { msg :: String, exit :: Bool}

type CurrentEventSourcing = EventSourcing MongoEventPersistor DummyStateManager

main :: IO ()
main = do
  options <- parseCommandLine
  pipe <- connect $ Host (fromMaybe "127.0.0.1" (Opts.mongoHostName options)) (maybe defaultPort (PortNumber . fromIntegral) (Opts.mongoPort options))
  eventSourcing <- initEventSourcing pipe
  runReaderT readEvalPrintLoop eventSourcing
  close pipe

initEventSourcing :: Pipe -> IO CurrentEventSourcing
initEventSourcing pipe =
  return $ newEventSourcing (newMongoEventPersistor pipe) newDummyStateManager

readEvalPrintLoop :: ReaderT CurrentEventSourcing IO ()
readEvalPrintLoop = do
  line <- liftIO getLine
  commandResult <- runCommand $ parseCommand line
  liftIO $ putStrLn $ msg commandResult
  unless (exit commandResult) readEvalPrintLoop

runCommand :: Either String Command -> ReaderT CurrentEventSourcing IO (CommandResult p s)
runCommand (Left errorString) = return $ CommandResult errorString False
runCommand (Right command) = case command of Exit -> return $ CommandResult "Bye" True
                                             WorldCommand cmd -> do
                                               eventSourcing <- ask
                                               liftIO $ do
                                                 newStatus <- execute eventSourcing cmd
                                                 return (CommandResult ("Command Executed: " ++ show newStatus)  False)

parseCommand :: String -> Either String Command
parseCommand = (>>= toCommand) . splitCommandAndArguments where
  toCommand (command, parameters) = commandToParseParameters command parameters

commandToParseParameters :: String -> [String] -> Either String Command
commandToParseParameters "exit" = exitParseParameters
commandToParseParameters "registerNaturalPerson" = registerNaturalPersonParseParameters
commandToParseParameters "registerFiscalPerson" = registerFiscalPersonParseParameters
commandToParseParameters _ = \_ -> Left "Command not recognized"

parseNameAndAddress :: String -> (Name -> Address -> World.Command World.World) -> [String] -> Either String Command
parseNameAndAddress _ createCommandFunction [name, address] = Right $ WorldCommand $ createCommandFunction (Name name) (Address address)
parseNameAndAddress errString _ _ = Left errString

registerNaturalPersonParseParameters :: [String] -> Either String Command
registerNaturalPersonParseParameters = parseNameAndAddress "syntax: registerNaturalPerson name address" World.RegisterNaturalPerson

registerFiscalPersonParseParameters :: [String] -> Either String Command
registerFiscalPersonParseParameters = parseNameAndAddress "syntax: registerFiscalPerson name address" World.RegisterFiscalPerson

exitParseParameters :: [String] -> Either String Command
exitParseParameters [] = Right Exit
exitParseParameters _ = Left "'exit' does not accept any parameter"

splitCommandAndArguments :: String -> Either String (String, [String])
splitCommandAndArguments command = case splittedList of [] -> Left ""
                                                        (x: xs) -> Right (x, xs)
                                   where
                                     splittedList = (split . dropBlanks . dropDelims . oneOf) " " command
