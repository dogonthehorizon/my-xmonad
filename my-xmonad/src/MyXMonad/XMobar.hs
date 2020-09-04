module MyXMonad.XMobar (
  render,
  spawn
) where

import Control.Monad.IO.Class      (MonadIO)
import Data.List                   (isInfixOf)
import System.IO                   (Handle, hPutStrLn)
import XMonad.Core                 (X)
import XMonad.Hooks.DynamicLog     (PP (..), dynamicLogWithPP, shorten, wrap,
                                    xmobarColor, xmobarPP)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run             (spawnPipe)

-- FIXME put this somewhere else, maybe a colors module
normalBorder = "#6986a0"
focusedBorder = "#ffca28" -- amber
foreground = "#DEE3E0"
background = "#263238" -- for material
winType = "#649A54" -- green

render :: Handle -> X ()
render handle =
    dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppOutput          = hPutStrLn handle
        , ppCurrent         = xmobarColor normalBorder "" . wrap "[" "]"
        , ppVisible         = xmobarColor foreground ""
        , ppHidden          = xmobarColor foreground ""
        , ppHiddenNoWindows = xmobarColor foreground ""
        , ppTitle           = xmobarColor "#84a0c6" "" . shorten 75
        , ppUrgent          = xmobarColor focusedBorder "" . wrap "!" ""
        --, ppOrder = \(workspaces : layout : t : _) -> [workspaces, layout, t]
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        , ppWsSep           = ""
        }

spawn :: (MonadIO m) => m Handle
spawn = spawnPipe "/home/ffreire/.local/bin/xmobar"
