module MyXMonad.Command.Background where

import Control.Monad.IO.Class (MonadIO)
import XMonad.Actions.SpawnOn (spawnHere)

backgroundLocation :: String
backgroundLocation = "$HOME/Documents/background/*"

spawn = spawnHere . show $  Background [Recursive, Randomize, FillBackground backgroundLocation]

data BackgroundFlags = Randomize | Recursive | FillBackground FilePath
  deriving Eq

instance Show BackgroundFlags where
  show Randomize          = "--randomize"
  show Recursive          = "--recursive"
  show (FillBackground f) = "--bg-fill " <> f

data Background = Background [BackgroundFlags]

instance Show Background where
  show (Background fs) =  "feh " <> foldr (mappend . mappend " " . show) "" fs
