module CmdOptions
( CmdOptions(..)
)
where

data CmdOptions = CmdOptions { worldName :: String
                       , mongoHostName :: Maybe String
                       , mongoPort :: Maybe Int
                       }
                       deriving (Eq, Show)
