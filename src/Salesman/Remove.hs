{-# LANGUAGE FlexibleContexts #-}

module Salesman.Remove
    ( remove
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (copyFile, getTemporaryDirectory)
import System.IO.Temp (createTempDirectory)
import System.Process (callCommand)
import Control.Monad (unless)

import Salesman.OptionTypes (Common(..))
import Salesman.Instance (downloadInstance)
import Salesman.Database (parseSalesmanJson, findNotInstalledPackages, findMissingDependencies, deletePackages, createDestructiveChangesSpecificComponents, writeSalesmanJson)
import Paths_salesman (getDataFileName)

remove :: (MonadReader Common m, MonadIO m) => [String] -> m ()
remove packages = do
    config <- reader optProperties

    -- let packageDatabase = read salesman json
    instanceDir <- downloadInstance
    packageDatabase <- parseSalesmanJson instanceDir

    -- make sure packages to be uninstalled are actually installed
    let notInstalledPackages = findNotInstalledPackages packageDatabase packages

    unless (null notInstalledPackages) $
        error $ "The following packages are not installed: " ++ show notInstalledPackages

    -- make sure depends are still satisfied after removing the packages
    let newPackageDatabase = deletePackages packageDatabase packages
    let missingDepends = findMissingDependencies newPackageDatabase

    unless (null missingDepends) $
        error $ "The following depends will not be satisfied after removal: " ++ show missingDepends

    tmpDir <- liftIO getTemporaryDirectory
    deployDir <- liftIO $ createTempDirectory tmpDir "salesman."

    -- create new salesman json
    writeSalesmanJson deployDir newPackageDatabase

    packageXml <- liftIO $ getDataFileName "package.xml"
    liftIO $ copyFile packageXml (deployDir ++ "/src/package.xml")

    -- deploy salesman json
    liftIO $ callCommand $ "java -jar ~/.salesman/tooling-force.com.jar --action=deployAll --projectPath=" ++ deployDir ++ " --responseFilePath=/dev/null --config=" ++ config

    -- remove the packages
    liftIO $ writeFile (instanceDir ++ "/src/specificComponents") (createDestructiveChangesSpecificComponents packageDatabase packages)

    -- deploy destructive changes
    liftIO $ callCommand $ "java -jar ~/.salesman/tooling-force.com.jar --action=deleteMetadata --projectPath=" ++ instanceDir ++ " --responseFilePath=/dev/null --config=" ++ config ++ " --specificComponents=" ++ instanceDir ++ "/src/specificComponents"

