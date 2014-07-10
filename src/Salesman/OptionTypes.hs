module Salesman.OptionTypes
    ( Command(..)
    , Common(..)
    , Options(..)
    ) where

data Command
    = Install [String]
    | Check
    | Upgrade
    | Remove [String]
    | List
    deriving (Show)

data Common = Common
    { optProperties :: FilePath
    } deriving (Show)

data Options = Options
    { optCommon :: Common
    , optCommand :: Command
    } deriving (Show)
