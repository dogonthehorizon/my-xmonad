module MyXMonad.XMobar (
  render,
  spawn
) where

import Control.Monad.IO.Class      (MonadIO)
import Data.List                   (isInfixOf)
import System.IO                   (Handle, hPutStrLn)
import XMonad.Hooks.DynamicLog     (PP (..), dynamicLogWithPP, wrap,
                                    xmobarColor, xmobarPP)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run             (spawnPipe)

-- FIXME put this somewhere else, maybe a colors module
normalBorder = "#6986a0"
focusedBorder = "#ffca28" -- amber
foreground = "#DEE3E0"
background = "#263238" -- for material
winType = "#649A54" -- green

--, ppTitle = xmobarColor myppTitle "" . shorten 30
render handle =
    dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppOutput          = hPutStrLn handle
        , ppCurrent         = xmobarColor normalBorder "" . wrap "[" "]"
        , ppVisible         = xmobarColor background ""
        , ppHidden          = xmobarColor background "" . wrap "+" ""
        , ppHiddenNoWindows = xmobarColor background ""
        , ppUrgent          = xmobarColor focusedBorder "" . wrap "!" ""
        , ppOrder = \(workspaces : layout : t : _) -> [workspaces, layout, t]
        , ppSep             = " "
        , ppWsSep           = ""
        , ppLayout          =
            xmobarColor winType ""
                . (\mode -> if "Tall" `isInfixOf` mode
                      then " tall "
                      else case mode of
                          "Full"     -> " full "
                          "ThreeCol" -> " 3col "
                          _          -> " ? "
                  )
        }

spawn :: (MonadIO m) => m Handle
spawn = spawnPipe "xmobar"
