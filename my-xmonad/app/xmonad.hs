import           Data.List                   (isInfixOf)
import           System.IO                   (Handle, hPutStrLn)
import           XMonad                      (Layout (..), X, XConfig (..),
                                              className, def, mod4Mask,
                                              sendMessage, title, xmonad, (-->),
                                              (.|.), (<&&>), (<+>), (=?))
import           XMonad.Actions.CycleWS      (nextWS, prevWS, shiftToNext,
                                              shiftToPrev)
import           XMonad.Hooks.DynamicLog     (pad)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageDocks    (avoidStruts, docks, manageDocks)
import           XMonad.Hooks.ManageHelpers  (doRectFloat)
import           XMonad.StackSet             (RationalRect (..))
import           XMonad.Util.EZConfig        (additionalKeys)
import           XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import           XMonad.Util.Run             (spawnPipe)

import qualified MyXMonad.Command.Background as Background
import qualified MyXMonad.Command.Compositor as Compositor
import           MyXMonad.KeyMapping
import           MyXMonad.Layout
import           MyXMonad.Scratchpad
import qualified MyXMonad.XMobar             as XMobar
import qualified Data.Color.Palette as Palette


backgroundImage = "$HOME/Documents/background/*"

statusFont = "'Ubuntu Mono derivative Powerline:size=14'"

borderWidthPx = 3 :: Int

-- | Generate workspace labels up to the given bound.
numWorkspaces :: Int -> [String]
numWorkspaces upperBound = pad . show <$> [1 .. upperBound]

main :: IO ()
main = do
    handle <- XMobar.spawn
    xmonad . ewmh . flip additionalKeys mKeys $ docks def
        { manageHook         = scratchpadHook <+> manageDocks <+> manageHook def
        , layoutHook         = layout
        , startupHook        = Background.spawn >> Compositor.spawn
        , workspaces         = numWorkspaces 5
        , terminal           = "kitty"
        , borderWidth        = fromIntegral borderWidthPx
        , focusedBorderColor = yellow
        , normalBorderColor  = "#6986A0" -- TODO migrate
        , logHook            = XMobar.render palette handle
        , modMask            = mod4Mask
        }
          where palette@(Palette.Palette {
                    Palette.foregroundColor = foregroundColor,
                    Palette.backgroundColor = backgroundColor,
                    Palette.yellow = yellow
                  }) = Palette.defaultPalette

