import           Data.List                        (isInfixOf)
import           System.IO                        (Handle, hPutStrLn)
import           XMonad                           (Layout (..),
                                                    X, XConfig (..),
                                                   className, def,
                                                   mod4Mask, sendMessage,
                                                   title, xmonad,
                                                   (-->), (.|.), (<&&>), (<+>),
                                                   (=?))
import           XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext,
                                                   shiftToPrev)
import           XMonad.Actions.SpawnOn           (spawnHere)
import           XMonad.Hooks.DynamicLog          (PP (..), dynamicLogWithPP,
                                                   dzenColor, pad)
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks,
                                                   manageDocks)
import           XMonad.Hooks.ManageHelpers       (doRectFloat)
import           XMonad.StackSet                  (RationalRect (..))
import           XMonad.Util.EZConfig             (additionalKeys)
import           XMonad.Util.Run                  (spawnPipe)
import           XMonad.Util.SpawnOnce            (spawnOnce)

import MyXMonad.Layout
import MyXMonad.KeyMapping


-- colors
normalBorder = "#6986a0"
focusedBorder = "#ffca28" -- amber
foreground = "#DEE3E0"
background = "#263238" -- for material
winType = "#649A54" -- green

backgroundImage = "$HOME/Documents/background/*"

statusFont = "'Ubuntu Mono derivative Powerline:size=14'"

borderWidthPx = 3 :: Int

logbar h = dynamicLogWithPP $ def
  { ppOutput          = hPutStrLn h
  , ppCurrent         = dzenColor foreground normalBorder
  , ppVisible         = dzenColor foreground background
  , ppHidden          = dzenColor foreground background
  , ppHiddenNoWindows = dzenColor foreground background
  , ppUrgent          = dzenColor foreground focusedBorder
  , ppOrder           = \(workspaces : layout : _) -> [workspaces, layout]
  , ppSep             = " "
  , ppWsSep           = ""
  , ppLayout          =
    dzenColor foreground winType
      . (\mode -> if "Tall" `isInfixOf` mode
          then " tall "
          else case mode of
            "Full"     -> " full "
            "ThreeCol" -> " 3col "
            _          -> " ? "
        )
  }

-- | Generate workspace labels up to the given bound.
numWorkspaces :: Int -> [String]
numWorkspaces upperBound = pad . show <$> [1 .. upperBound]

myStartupHook =
  spawnHere ("feh --randomize --recursive --bg-fill " <> backgroundImage)
    >> spawnOnce "compton"
    >> spawnHere
         ("i3status | dzen2 -ta r -x 480 -h 48 -w 3360 -fn " ++ statusFont)

main :: IO ()
main = do
  bar <- spawnPipe ("dzen2 -ta l -p -w 480 -h 48 -fn " ++ statusFont)
  xmonad
    .                ewmh
    $                docks def
                       { manageHook         = manageDocks <+> manageHook def
                       , layoutHook         = layout
                       , startupHook        = myStartupHook
                       , workspaces         = numWorkspaces 5
                       , terminal           = "kitty"
                       , borderWidth        = fromIntegral borderWidthPx
                       , focusedBorderColor = focusedBorder
                       , normalBorderColor  = normalBorder
                       , logHook            = logbar bar
                       , modMask            = mod4Mask
                       }
    `additionalKeys` mKeys
