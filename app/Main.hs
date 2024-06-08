{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Dash
import Data.List (isSuffixOf, sortOn)
import Database.SQLite.Simple (close, open)
import Options.Applicative
import Symbol
import System.Directory
import System.Environment

listDocsets :: FilePath -> IO [FilePath]
listDocsets = fmap (map (\dp -> take (length dp - 7) dp) . filter ("docset" `isSuffixOf`)) . getDirectoryContents

docsetDbPath :: FilePath -> FilePath -> FilePath
docsetDbPath rootPath docset = rootPath <> docset <> ".docset/Contents/Resources/docSet.dsidx"

docsetSymbolPath :: FilePath -> Symbol -> FilePath
docsetSymbolPath rootPath (Symbol {docset = d, symbolPath = path}) = rootPath <> d <> ".docset/Contents/Resources/Documents/" <> path

data CLICommand = List | Lookup String String

data CLIOptions = CLIOptions FilePath CLICommand

dashit :: Parser CLIOptions
dashit =
  CLIOptions
    <$> strArgument (metavar "DB_ROOT_PATH")
    <*> subparser
      ( command "list" (info listOptions (progDesc "List all symbols in all docsets"))
          <> command "lookup" (info lookupOptions (progDesc "Lookup SYMBOL in DOCSET"))
      )
  where
    listOptions = pure List
    lookupOptions = Lookup <$> strArgument (metavar "DOCSET") <*> strArgument (metavar "SYMBOL")

execDashCommand :: FilePath -> CLICommand -> IO ()
execDashCommand dashRootPath List = do
  docsets <- listDocsets dashRootPath
  globalSymbols <-
    concat
      <$> mapM
        ( \docset -> do
            conn <- open (docsetDbPath dashRootPath docset)
            symbols <- readSymbols conn docset
            close conn
            return symbols
        )
        docsets
  mapM_ (\Symbol {docset = d, symbolName = n} -> putStrLn $ d <> " " <> n) globalSymbols
execDashCommand dashRootPath (Lookup docset symbolQuery) = do
  conn <- open (docsetDbPath dashRootPath docset)
  symbolResult <- lookupSymbol conn docset symbolQuery
  case symbolResult of
    Just symbol -> putStrLn (docsetSymbolPath dashRootPath symbol)
    Nothing -> return ()

main :: IO ()
main = do
  (CLIOptions dashRootPath dashCommand) <- execParser dashitCLI
  execDashCommand dashRootPath dashCommand
  where
    dashitCLI = info dashit (fullDesc <> progDesc "Query a library of dash docs at DB_ROOT_PATH" <> header "dashit - a simple CLI interface to dash docsets.")
