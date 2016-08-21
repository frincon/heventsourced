module CommandLineParser
( parseCommandLine
)
where

import CmdOptions (CmdOptions(CmdOptions))
import Options.Applicative

parseCommandLine :: IO CmdOptions
parseCommandLine = execParser opts where
  opts = info (helper <*> parser)
    (  fullDesc
    <> progDesc "Run the ERP"
    <> header "erp - a example of an erp with haskell"
    )

parser :: Parser CmdOptions
parser = CmdOptions
         <$> worldName
         <*> mongoHost
         <*> mongoPort

worldName :: Parser String
worldName = strOption
    (  long "world"
    <> short 'w'
    <> metavar "WORLDNAME"
    <> help "Use the wold with name WORLDNAME" )

mongoHost :: Parser (Maybe String)
mongoHost = optional (strOption
    (  long "mongoHost"
    <> short 'h'
    <> metavar "HOSTNAME"
    <> help "Connect to mongo using HOSTNAME" ))

mongoPort :: Parser (Maybe Int)
mongoPort = optional (option auto
    (  long "mongoPort"
    <> short 'p'
    <> metavar "PORT"
    <> help "Connect to mongo using PORT" ))
