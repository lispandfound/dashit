{-# LANGUAGE OverloadedStrings #-}

module Dash (readSymbols, lookupSymbol) where

import Database.SQLite.Simple
import Symbol

intoDocset :: String -> FreeSymbol -> Symbol
intoDocset docsetName (FreeSymbol {freeType = t, freeName = n, freePath = p}) = Symbol t n p docsetName

readSymbols :: Connection -> String -> IO [Symbol]
readSymbols conn docsetName = map (intoDocset docsetName) <$> query_ conn "SELECT name, type, path FROM searchIndex"

lookupSymbol :: Connection -> String -> String -> IO (Maybe Symbol)
lookupSymbol conn docsetName symbolName = headMay . map (intoDocset docsetName) <$> query conn "SELECT name, type, path FROM searchIndex WHERE name = ?" (Only symbolName)
  where
    headMay [] = Nothing
    headMay (x : _) = Just x
