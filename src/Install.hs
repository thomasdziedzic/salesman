{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Install
    ( install
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, copyFile)
import System.Process (callCommand, readProcess)
import Data.Aeson (eitherDecode, FromJSON, encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import System.IO.Temp (createTempDirectory)
import Data.Char (isSpace)

import OptionTypes (Command(..), Common(..), Options(..))
import Paths_salesman (getDataFileName)
import Instance (downloadInstance)
import Database (PackageDatabase(..), PackageDatabaseEntry(..), databaseContainsPackage, findInstalledPackages, doesSalesmanJsonExist, parseSalesmanJson)
import Process (readCommand)

install :: (MonadReader Common m, MonadIO m) => [String] -> m ()
install packages = do
    config <- reader $ optProperties

    home <- liftIO getHomeDirectory
    let salesmanDir = home ++ "/" ++ ".salesman"
        cacheDir = salesmanDir ++ "/" ++ "cache"

    liftIO $ createDirectoryIfMissing True cacheDir

    -- check if the instance doesn't have these packages already
    -- installed in the salesman_json file
    instanceDir <- downloadInstance
    salesmanJsonExists <- doesSalesmanJsonExist instanceDir
    packageDatabase <- if salesmanJsonExists
                           then parseSalesmanJson instanceDir
                           else return $ PackageDatabase []
    let existingPackages = findInstalledPackages packageDatabase packages
    if not . null $ existingPackages
        then error $ "Packages already installed: " ++ show existingPackages
        else return ()

    liftIO $ putStrLn "downloading repo"

    -- download github repository containing repo index
    liftIO $ downloadRepo "gostrc" "salesman-db"

    tmpDir <- liftIO getTemporaryDirectory
    targetDir <- liftIO $ createTempDirectory tmpDir "salesman."

    liftIO $ putStrLn "downloading package"

    -- get each package and download it locally
    packageEntries <- liftIO $ mapM (downloadPackage targetDir) packages

    -- copy simple package xml for consistency
    packageXml <- liftIO $ getDataFileName "package.xml"
    liftIO $ copyFile packageXml (targetDir ++ "/src/package.xml")

    -- update salesman_json with new data
    liftIO $ createDirectoryIfMissing True (targetDir ++ "/src/staticresources")
    liftIO $ BL.writeFile (targetDir ++ "/src/staticresources/salesman_json.resource") (encode (PackageDatabase (packageEntries ++ entries packageDatabase)))

    dbMeta <- liftIO $ getDataFileName "salesman_json.resource-meta.xml"
    liftIO $ copyFile dbMeta (targetDir ++ "/src/staticresources/salesman_json.resource-meta.xml")

    -- deploy
    liftIO $ callCommand $ "java -jar tooling-force.com-0.1.4.2-getCompilerErrors-fix.jar --action=deployAll --projectPath=" ++ targetDir ++ " --responseFilePath=/dev/null --config=" ++ config

downloadRepo :: String -> String -> IO ()
downloadRepo user repo = do
    userDir <- getUserDir user
    repoDir <- getRepoDir user repo

    createDirectoryIfMissing True userDir

    repoDirExists <- doesDirectoryExist repoDir

    if repoDirExists
         then callCommand $ "cd " ++ repoDir ++ " && git checkout master && git pull"
         else callCommand $ "git clone " ++ githubUrl user repo ++ " " ++ repoDir

githubUrl :: String -> String -> String
githubUrl user repo = "https://github.com/" ++ user ++ "/" ++ repo ++ ".git"

getRepoDir :: String -> String -> IO String
getRepoDir user repo = do
    userDir <- getUserDir user

    let repoDir = userDir ++ "/" ++ repo

    return repoDir

getUserDir :: String -> IO String
getUserDir user = do
    home <- getHomeDirectory

    let salesmanDir = home ++ "/" ++ ".salesman"
        cacheDir = salesmanDir ++ "/" ++ "cache"
        userDir = cacheDir ++ "/" ++ user

    return userDir

downloadPackage :: String -> String -> IO PackageDatabaseEntry
downloadPackage targetDir package = do
    -- read from package file in index
    repoDir <- getRepoDir "gostrc" "salesman-db"
    let indexDir = repoDir ++ "/index"
        packageFile = indexDir ++ "/" ++ package ++ ".json"

    fileContents <- BL.readFile packageFile

    index <- case eitherDecode fileContents of
                 Left errorMessage -> error errorMessage
                 Right index@PackageIndex{} -> return index

    -- download repo
    downloadRepo (githubUser index) (githubRepo index)

    packageDir <- getRepoDir (githubUser index) (githubRepo index)
    let packageSrc = packageDir ++ "/src"

    stdout <- readCommand $ "cd " ++ packageDir ++ " && git describe --abbrev=0"
    putStrLn $ "latest tag is: \"" ++ stdout ++ "\""
    let latestVersion = trim stdout
    putStrLn $ "latest tag is: \"" ++ latestVersion ++ "\""

    if null latestVersion
        then error $ "There are no tags for " ++ package
        else callCommand $ "cd " ++ packageDir ++ " && git checkout " ++ latestVersion

    stdout <- readCommand $ "cd " ++ packageDir ++ " && find src | grep '\\..*$' | grep -v '\\.xml$'"
    let files = lines stdout

    packageDescriptionFile <- BL.readFile $ packageDir ++ "/package.json"

    packageDescription <- case eitherDecode packageDescriptionFile of
                              Left errorMessage -> error errorMessage
                              Right desc -> return desc

    -- stage files
    callCommand $ "cp -r " ++ packageSrc ++ " " ++ targetDir

    return $ PackageDatabaseEntry package latestVersion files (pkgDepends packageDescription)

data PackageIndex = PackageIndex
    { githubUser :: String
    , githubRepo :: String
    } deriving (Show, Generic)

instance FromJSON PackageIndex

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data PackageDescription = PackageDescription
    { pkgDepends :: [String]
    } deriving (Show, Generic)

instance FromJSON PackageDescription
