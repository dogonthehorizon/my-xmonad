module MyXMonad.XMobar (
  render,
  spawn
) where

import Control.Monad.IO.Class      (MonadIO)
import MyXMonad.Colors             (blueGray, focusedBorder, foreground,
                                    normalBorder)
import System.IO                   (Handle, hPutStrLn)
import XMonad.Core                 (X)
import XMonad.Hooks.DynamicLog     (PP (..), dynamicLogWithPP, shorten, wrap,
                                    xmobarColor, xmobarPP, pad)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run             (spawnPipe)
import qualified Data.Color.Palette as Palette

-- FIXME: accept a color palette in the render method

render :: Handle -> X ()
render handle =
    dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppOutput          = hPutStrLn handle
        , ppCurrent         = xmobarColor (Palette.yellow Palette.defaultPalette) "" . wrap "{" "}"
        , ppVisible         = xmobarColor (Palette.foregroundColor Palette.defaultPalette) ""
        , ppTitle           = xmobarColor (Palette.foregroundColor Palette.defaultPalette) "" . shorten 75
        , ppUrgent          = xmobarColor (Palette.red Palette.defaultPalette) "" . wrap "!" ""
        , ppOrder = \(workspaces : layout : title : _) -> [workspaces, layout, title]
        , ppWsSep           = ""
        , ppSep           = pad "|"
        }

spawn :: (MonadIO m) => m Handle
spawn = spawnPipe "/home/ffreire/.local/bin/my-xmobar"
