{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Salesman.Database
    ( PackageDatabaseEntry(..)
    , PackageDatabase(..)
    , parseSalesmanJson
    , doesSalesmanJsonExist
    , findInstalledPackages
    , findNotInstalledPackages
    , findMissingDependencies
    , deletePackages
    , createDestructiveChangesSpecificComponents
    ) where

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (doesFileExist)
import Data.List ((\\))
import qualified Data.Set as S

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

parseSalesmanJson :: (MonadReader r m, MonadIO m) => FilePath -> m PackageDatabase
parseSalesmanJson targetDir = do
    salesmanJson <- liftIO $ BL.readFile $ targetDir ++ "/src/staticresources/salesman_json.resource"

    case eitherDecode salesmanJson of
        Left errorMessage -> error errorMessage
        Right db -> return db

doesSalesmanJsonExist :: (MonadIO m) => FilePath -> m Bool
doesSalesmanJsonExist instanceDir =
    liftIO $ doesFileExist $ instanceDir ++ "/src/staticresources/salesman_json.resource"

databaseContainsPackage :: PackageDatabase -> String -> Bool
databaseContainsPackage (PackageDatabase { .. }) packageName =
    not . null $ filter (\x -> packageName == name x) entries

findInstalledPackages :: PackageDatabase -> [String] -> [String]
findInstalledPackages packageDatabase = filter (databaseContainsPackage packageDatabase)

findNotInstalledPackages :: PackageDatabase -> [String] -> [String]
findNotInstalledPackages packageDatabase = filter (not . databaseContainsPackage packageDatabase)

findMissingDependencies :: PackageDatabase -> [String]
findMissingDependencies packageDatabase = packageDepends \\ packageNames
  where
    packageDatabaseEntries = entries packageDatabase
    packageNames = map name packageDatabaseEntries
    packageDepends = concatMap depends packageDatabaseEntries

deletePackages :: PackageDatabase -> [String] -> PackageDatabase
deletePackages packageDatabase packages =
    PackageDatabase newEntries
  where
    packagesSet = S.fromList packages
    newEntries = filter (\pe -> not (S.member (name pe) packagesSet)) (entries packageDatabase)

findPackageEntries :: PackageDatabase -> [String] -> [PackageDatabaseEntry]
findPackageEntries packageDatabase packages = foundEntries
  where
    packagesSet = S.fromList packages
    foundEntries = filter (\pe -> S.member (name pe) packagesSet) (entries packageDatabase)

createDestructiveChangesSpecificComponents :: PackageDatabase -> [String] -> String
createDestructiveChangesSpecificComponents packageDatabase packages =
  unlines $ concatMap files $ findPackageEntries packageDatabase packages
