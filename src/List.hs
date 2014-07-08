{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module List
    ( list
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))

import OptionTypes (Command(..), Common(..), Options(..))
import Database (parseSalesmanJson, PackageDatabase(..), PackageDatabaseEntry(..))
import Paths_salesman (getDataFileName)
import Instance (downloadInstance)

list :: (MonadReader Common m, MonadIO m) => m ()
list = do
    targetDir <- downloadInstance

    packageDatabase <- parseSalesmanJson targetDir

    printPackageDatabase packageDatabase

printPackageDatabase :: (MonadIO m) => PackageDatabase -> m ()
printPackageDatabase packageDatabase =
    liftIO $ mapM_ (putStrLn . getPackageEntryLine) (entries packageDatabase)

getPackageEntryLine :: PackageDatabaseEntry -> String
getPackageEntryLine PackageDatabaseEntry { .. } = name ++ " : " ++ version
