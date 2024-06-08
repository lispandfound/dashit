module Symbol (Symbol (..), FreeSymbol (..)) where

import Database.SQLite.Simple.FromRow

instance FromRow FreeSymbol where
  fromRow = FreeSymbol <$> field <*> field <*> field

data FreeSymbol = FreeSymbol
  { freeName :: String,
    freeType :: String,
    freePath :: String
  }

data Symbol = Symbol
  { symbolType :: String,
    symbolName :: String,
    symbolPath :: String,
    docset :: String
  }
