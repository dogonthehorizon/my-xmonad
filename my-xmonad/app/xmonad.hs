import           Data.List                   (isInfixOf)
import           System.IO                   (Handle, hPutStrLn)
import           XMonad                      (Layout (..), X, XConfig (..),
                                              className, def, mod4Mask,
                                              sendMessage, title, xmonad, (-->),
                                              (.|.), (<&&>), (<+>), (=?))
import           XMonad.Actions.CycleWS      (nextWS, prevWS, shiftToNext,
                                              shiftToPrev)
import           XMonad.Actions.SpawnOn      (spawnHere)
import           XMonad.Hooks.DynamicLog     (pad)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageDocks    (avoidStruts, docks, manageDocks)
import           XMonad.Hooks.ManageHelpers  (doRectFloat)
import           XMonad.StackSet             (RationalRect (..))
import           XMonad.Util.EZConfig        (additionalKeys)
import           XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import           XMonad.Util.Run             (spawnPipe)
import           XMonad.Util.SpawnOnce       (spawnOnce)

import           MyXMonad.KeyMapping
import           MyXMonad.Layout
import           MyXMonad.Scratchpad
import qualified MyXMonad.XMobar             as XMobar


-- colors
normalBorder = "#6986a0"
focusedBorder = "#ffca28" -- amber
foreground = "#DEE3E0"
background = "#263238" -- for material
winType = "#649A54" -- green

backgroundImage = "$HOME/Documents/background/*"

statusFont = "'Ubuntu Mono derivative Powerline:size=14'"

borderWidthPx = 3 :: Int

-- | Generate workspace labels up to the given bound.
numWorkspaces :: Int -> [String]
numWorkspaces upperBound = pad . show <$> [1 .. upperBound]

myStartupHook =
    spawnHere ("feh --randomize --recursive --bg-fill " <> backgroundImage)
        >> spawnOnce "picom"

main :: IO ()
main = do
    handle <- XMobar.spawn
    xmonad . ewmh . flip additionalKeys mKeys $ docks def
        { manageHook         = scratchpadHook <+> manageDocks <+> manageHook def
        , layoutHook         = layout
        , startupHook        = myStartupHook
        , workspaces         = numWorkspaces 5
        , terminal           = "kitty"
        , borderWidth        = fromIntegral borderWidthPx
        , focusedBorderColor = focusedBorder
        , normalBorderColor  = normalBorder
        , logHook            = XMobar.render handle
        , modMask            = mod4Mask
        }
