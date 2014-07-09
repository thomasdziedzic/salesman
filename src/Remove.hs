{-# LANGUAGE FlexibleContexts #-}

module Remove
    ( remove
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (copyFile, createDirectoryIfMissing, getTemporaryDirectory)
import System.IO.Temp (createTempDirectory)
import System.Process (callCommand)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)

import OptionTypes (Common(..))
import Instance (downloadInstance)
import Database (PackageDatabase(..), doesSalesmanJsonExist, parseSalesmanJson, findNotInstalledPackages, findMissingDependencies, deletePackages, createDestructiveChangesSpecificComponents)
import Paths_salesman (getDataFileName)

remove :: (MonadReader Common m, MonadIO m) => [String] -> m ()
remove packages = do
    config <- reader $ optProperties

    -- let packageDatabase = read salesman json
    instanceDir <- downloadInstance
    salesmanJsonExists <- doesSalesmanJsonExist instanceDir
    packageDatabase <- if salesmanJsonExists
                           then parseSalesmanJson instanceDir
                           else return $ PackageDatabase []

    -- make sure packages to be uninstalled are actually installed
    let notInstalledPackages = findNotInstalledPackages packageDatabase packages

    if null notInstalledPackages
        then return ()
        else error $ "The following packages are not installed: " ++ show notInstalledPackages

    -- make sure depends are still satisfied after removing the packages
    let newPackageDatabase = deletePackages packageDatabase packages
    let missingDepends = findMissingDependencies newPackageDatabase

    if null missingDepends
        then return ()
        else error $ "The following depends will not be satisfied after removal: " ++ show missingDepends

    tmpDir <- liftIO getTemporaryDirectory
    deployDir <- liftIO $ createTempDirectory tmpDir "salesman."

    -- create new salesman json
    liftIO $ createDirectoryIfMissing True (deployDir ++ "/src/staticresources")
    liftIO $ BL.writeFile (deployDir ++ "/src/staticresources/salesman_json.resource") (encode newPackageDatabase)

    dbMeta <- liftIO $ getDataFileName "salesman_json.resource-meta.xml"
    liftIO $ copyFile dbMeta (deployDir ++ "/src/staticresources/salesman_json.resource-meta.xml")

    packageXml <- liftIO $ getDataFileName "package.xml"
    liftIO $ copyFile packageXml (deployDir ++ "/src/package.xml")

    -- deploy salesman json
    liftIO $ callCommand $ "java -jar tooling-force.com-0.1.4.2-getCompilerErrors-fix.jar --action=deployAll --projectPath=" ++ deployDir ++ " --responseFilePath=/dev/null --config=" ++ config

    -- remove the packages
    liftIO $ writeFile (instanceDir ++ "/src/specificComponents") (createDestructiveChangesSpecificComponents packageDatabase packages)

    -- copy empty package.xml
    -- http://www.salesforce.com/us/developer/docs/daas/Content/daas_destructive_changes.htm
    --emptyPackageXml <- liftIO $ getDataFileName "emptyPackage.xml"
    --liftIO $ copyFile emptyPackageXml (instanceDir ++ "/src/package.xml")

    -- deploy destructive changes
    liftIO $ callCommand $ "java -jar tooling-force.com-0.1.4.2-getCompilerErrors-fix.jar --action=deleteMetadata --projectPath=" ++ instanceDir ++ " --responseFilePath=/dev/null --config=" ++ config ++ " --specificComponents=" ++ instanceDir ++ "/src/specificComponents"

