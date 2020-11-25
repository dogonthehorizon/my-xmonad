module MyXMonad.XMobar (
  render,
  spawn
) where

import Control.Monad.IO.Class      (MonadIO)
import MyXMonad.Colors             (focusedBorder, foreground, normalBorder, blueGray)
import System.IO                   (Handle, hPutStrLn)
import XMonad.Core                 (X)
import XMonad.Hooks.DynamicLog     (PP (..), dynamicLogWithPP, shorten, wrap,
                                    xmobarColor, xmobarPP)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run             (spawnPipe)

render :: Handle -> X ()
render handle =
    dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppOutput          = hPutStrLn handle
        , ppCurrent         = xmobarColor focusedBorder "" . wrap "[" "]"
        , ppVisible         = xmobarColor foreground ""
        , ppHidden          = xmobarColor foreground ""
        , ppHiddenNoWindows = xmobarColor foreground ""
        , ppTitle           = xmobarColor blueGray"" . shorten 75
        , ppUrgent          = xmobarColor focusedBorder "" . wrap "!" ""
        , ppOrder = \(workspaces : layout : t : _) -> [workspaces, layout, t]
        , ppWsSep           = ""
        }

spawn :: (MonadIO m) => m Handle
spawn = spawnPipe "/home/ffreire/.local/bin/my-xmobar"
