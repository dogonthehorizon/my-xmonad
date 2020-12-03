module Data.Color.Palette (
  Palette(..),
  defaultPalette
) where

data Palette = Palette {
    backgroundColor :: String,
    foregroundColor :: String,
    red             :: String,
    green :: String,
    yellow :: String
  }

defaultPalette :: Palette
defaultPalette = Palette
  { backgroundColor = "#263238"
  , foregroundColor = "#DEE3E0"
  , red             = "#DC233F"
  , green           = "#859900"
  , yellow          = "#B58900"
  }
