{-# LANGUAGE FlexibleContexts #-}
module Instance
    ( downloadInstance
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Process (callCommand)
import System.IO.Temp (createTempDirectory)
import System.Directory (copyFile, createDirectoryIfMissing, getTemporaryDirectory)

import OptionTypes (Common(..))
import Paths_salesman (getDataFileName)

downloadInstance :: (MonadReader Common m, MonadIO m) => m FilePath
downloadInstance = do
    properties <- reader $ optProperties

    tmpDir <- liftIO getTemporaryDirectory
    targetDir <- liftIO $ createTempDirectory tmpDir "salesman."

    let srcDir = targetDir ++ "/src"

    liftIO $ createDirectoryIfMissing True srcDir

    packageXml <- liftIO $ getDataFileName "package.xml"

    liftIO $ copyFile packageXml (srcDir ++ "/package.xml")

    liftIO $ callCommand $ "java -jar tooling-force.com-0.1.4.2-getCompilerErrors-fix.jar --action=refresh --projectPath=" ++ targetDir ++ " --responseFilePath=/dev/null --skipModifiedFilesCheck=true --config=" ++ properties ++ " --tempFolderPath=" ++ targetDir

    let refreshDir = targetDir ++ "/refresh"
        unpackagedDir = refreshDir ++ "/unpackaged"

    liftIO $ callCommand $ "cp -r " ++ unpackagedDir ++ "/* " ++ srcDir
    liftIO $ callCommand $ "rm -r " ++ refreshDir

    return targetDir
