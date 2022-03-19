module MyXMonad.XMobar (
  render,
  spawn
) where

import Control.Monad.IO.Class      (MonadIO)
import System.IO                   (Handle, hPutStrLn)
import XMonad.Core                 (X)
import XMonad.Hooks.DynamicLog     (PP (..), dynamicLogWithPP, shorten, wrap,
                                    xmobarColor, xmobarPP, pad)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run             (spawnPipe)
import Data.Color.Palette (Palette)
import qualified Data.Color.Palette as Palette

render :: Palette -> Handle -> X ()
render palette handle =
    dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppOutput          = hPutStrLn handle
        , ppCurrent         = xmobarColor  yellow "" . wrap "{" "}"
        , ppVisible         = xmobarColor foregroundColor ""
        , ppTitle           = xmobarColor foregroundColor "" . shorten 75
        , ppUrgent          = xmobarColor red "" . wrap "!" ""
        , ppOrder = \(workspaces : layout : title : _) -> [workspaces, layout, title]
        , ppWsSep           = ""
        , ppSep           = pad "|"
        }
          where Palette.Palette {
                    Palette.yellow = yellow,
                    Palette.foregroundColor = foregroundColor,
                    Palette.red = red
                  } = palette

spawn :: (MonadIO m) => m Handle
spawn = spawnPipe "/home/ffreire/.local/bin/my-xmobar"
