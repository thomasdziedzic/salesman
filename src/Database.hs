{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database
    ( parseSalesmanJson
    , doesSalesmanJsonExist
    , databaseContainsPackage
    , findInstalledPackages
    , PackageDatabaseEntry(..)
    , PackageDatabase(..)
    ) where

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (doesFileExist)

parseSalesmanJson :: (MonadReader r m, MonadIO m) => FilePath -> m PackageDatabase
parseSalesmanJson targetDir = do
    salesmanJson <- liftIO $ BL.readFile $ targetDir ++ "/src/staticresources/salesman_json.resource"

    packageDatabase <- case eitherDecode salesmanJson of
                           Left errorMessage -> error errorMessage
                           Right db -> return db

    return packageDatabase

doesSalesmanJsonExist :: (MonadIO m) => FilePath -> m Bool
doesSalesmanJsonExist instanceDir =
    liftIO $ doesFileExist $ instanceDir ++ "/src/staticresources/salesman_json.resource"

databaseContainsPackage :: PackageDatabase -> String -> Bool
databaseContainsPackage (PackageDatabase { .. }) packageName =
    not . null $ filter (\x -> packageName == (name x)) entries

findInstalledPackages :: PackageDatabase -> [String] -> [String]
findInstalledPackages packageDatabase packages =
    filter (databaseContainsPackage packageDatabase) packages

data PackageDatabaseEntry = PackageDatabaseEntry
    { name :: String
    , version :: String
    , files :: [String]
    , depends :: [String]
    } deriving (Show, Generic)

instance FromJSON PackageDatabaseEntry
instance ToJSON PackageDatabaseEntry

data PackageDatabase = PackageDatabase
    { entries :: [PackageDatabaseEntry]
    } deriving (Show, Generic)

instance FromJSON PackageDatabase
instance ToJSON PackageDatabase
