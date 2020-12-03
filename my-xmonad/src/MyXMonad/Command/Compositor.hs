module MyXMonad.Command.Compositor where

import Control.Monad.IO.Class (MonadIO)
import XMonad.Util.SpawnOnce  (spawnOnce)

spawn = spawnOnce . show $ Picom [Config "~/.config/picom/picom.config"]

-- | Definitions for supported compositors.
data Compositor = Picom [PicomFlag]

instance Show Compositor where
  show (Picom fs) = mappend "picom " $ foldr (mappend . mappend " " . show) "" fs

-- | Definitions for supported Picom flags.
data PicomFlag = Config String

instance Show PicomFlag where
  show (Config s) = "--config " <> s
