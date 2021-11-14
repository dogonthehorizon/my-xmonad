module Data.Color.Palette (
  Palette(..),
  defaultPalette,
  pastelle
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

-- | From: https://colorbox.io/?c0=%26p%24s%24%3D11%26p%24h%24st%24%3D333%26p%24h%24e%24%3D335%26p%24h%24c%24%3Deqo%26p%24sa%24st%24%3D0.01%26p%24sa%24e%24%3D0.82%26p%24sa%24r%24%3D1%26p%24sa%24c%24%3Deqo%26p%24b%24st%24%3D1%26p%24b%24e%24%3D0.08%26p%24b%24c%24%3Dl%26o%24n%24%3DNew+Color%26o%24ro%24%3Dcw%26o%24ms%24%3D0%2C1&c1=%26p%24s%24%3D11%26p%24h%24st%24%3D130%26p%24h%24e%24%3D139%26p%24h%24c%24%3Deqo%26p%24sa%24st%24%3D0.1%26p%24sa%24e%24%3D0.79%26p%24sa%24r%24%3D1%26p%24sa%24c%24%3Deqo%26p%24b%24st%24%3D1%26p%24b%24e%24%3D0.1%26p%24b%24c%24%3Dl%26o%24n%24%3DNew+Color%26o%24ro%24%3Dcw%26o%24ms%24%3D0%2C1&c2=%26p%24s%24%3D11%26p%24h%24st%24%3D57%26p%24h%24e%24%3D59%26p%24h%24c%24%3Deqo%26p%24sa%24st%24%3D0.02%26p%24sa%24e%24%3D0.91%26p%24sa%24r%24%3D1%26p%24sa%24c%24%3Deqo%26p%24b%24st%24%3D1%26p%24b%24e%24%3D0.05%26p%24b%24c%24%3Dl%26o%24n%24%3DNew+Color%26o%24ro%24%3Dcw%26o%24ms%24%3D0%2C1
pastelle :: Palette
pastelle = Palette
  { backgroundColor = "#FFFCFE" -- rose
  , foregroundColor = "#14040B" -- black (w/ red)
  , red             = "#B9678A"
  , green           = "#63BA79"
  , yellow          = "#B6B35D"
  }
