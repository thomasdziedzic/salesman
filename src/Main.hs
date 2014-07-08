module Main where

import Control.Monad.Reader (runReaderT)
import Control.Applicative (pure, (<$>), (<**>), some, (<*>))
import Data.Monoid ((<>))
import Options.Applicative (subparser, command, info, argument, str, progDesc, helper, idm, execParser, metavar, Parser, ParserInfo, strOption, help, long, short)

import OptionTypes (Command(..), Common(..), Options(..))
import Install (install)
import List (list)

commonParser :: Parser Common
commonParser = Common
    <$> strOption
        ( long "properties"
       <> short 'p'
       <> metavar "FILE"
       <> help "The properties file containing the instance info"
        )

commandParser :: Parser Command
commandParser = subparser
    ( command "install"
        (info (Install <$> some (argument str (metavar "PACKAGE...")))
            (progDesc "Install packages"))
   <> command "upgrade"
        (info (pure Upgrade)
            (progDesc "Upgrade packages"))
   <> command "remove"
        (info (Remove <$> some (argument str (metavar "PACKAGE...")))
            (progDesc "Remove packages"))
   <> command "list"
        (info (pure List)
            (progDesc "List installed packages with versions"))
    )

optionsParser :: Parser Options
optionsParser = Options
    <$> commonParser
    <*> commandParser

run :: Options -> IO ()
run Options { optCommon = optCommon, optCommand = Install pkgs } = runReaderT (install pkgs) optCommon
{-
run Upgrade = putStrLn "Upgrading"
run (Remove pkgs) = putStrLn $ "Removing: " ++ show pkgs
-}
run Options { optCommon = optCommon, optCommand = List } = runReaderT list optCommon
run options = print options

opts :: ParserInfo Options
opts = info (optionsParser <**> helper) idm

main :: IO ()
main = execParser opts >>= run
