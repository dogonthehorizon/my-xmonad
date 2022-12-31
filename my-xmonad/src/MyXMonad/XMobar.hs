{-# LANGUAGE FlexibleContexts #-}

module MyXMonad.XMobar (render) where

import Control.Monad.IO.Class (MonadIO)
import Data.Color.Palette (Palette)
import qualified Data.Color.Palette as Palette
import Graphics.X11.Types (Window)
import XMonad.Core (LayoutClass, X, XConfig)
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarProp, withSB)
import XMonad.Hooks.StatusBar.PP
  ( PP (..),
    dynamicLogWithPP,
    filterOutWsPP,
    pad,
    shorten,
    wrap,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Util.Run (spawnPipe)

render :: LayoutClass l Window => Palette -> XConfig l -> XConfig l
render palette =
  withSB $
    statusBarProp "$HOME/.local/bin/my-xmobar" $
      pure $
        filterOutWsPP ["NSP"] $
          xmobarPP
            { ppCurrent = xmobarColor yellow "" . wrap "{" "}",
              ppVisible = xmobarColor foregroundColor "",
              ppTitle = xmobarColor foregroundColor "" . shorten 75,
              ppUrgent = xmobarColor red "" . wrap "!" "",
              ppOrder = \(workspaces : layout : title : _) -> [workspaces, layout, title],
              ppWsSep = "",
              ppSep = pad "|"
            }
  where
    Palette.Palette
      { Palette.yellow = yellow,
        Palette.foregroundColor = foregroundColor,
        Palette.red = red
      } = palette
