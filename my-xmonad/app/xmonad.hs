import qualified Data.Color.Palette as Palette
import Data.List (isInfixOf)
import qualified MyXMonad.Command.Background as Background
import qualified MyXMonad.Command.Compositor as Compositor
import MyXMonad.KeyMapping (mKeys)
import MyXMonad.Layout (layout)
import MyXMonad.Scratchpad (scratchpadHook, spotifyFloatingHook)
import qualified MyXMonad.XMobar as XMobar
import System.IO (Handle, hPutStrLn)
import XMonad
  ( Layout (..),
    X,
    XConfig (..),
    className,
    def,
    mod4Mask,
    sendMessage,
    title,
    xmonad,
    (-->),
    (.|.),
    (<&&>),
    (<+>),
    (=?),
  )
import XMonad.Actions.CycleWS
  ( nextWS,
    prevWS,
    shiftToNext,
    shiftToPrev,
  )
import XMonad.Hooks.DynamicLog (pad)
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.StackSet (RationalRect (..))
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run (spawnPipe)

borderWidthPx = 3 :: Int

-- | Generate workspace labels up to the given bound.
numWorkspaces :: Int -> [String]
numWorkspaces upperBound = pad . show <$> [1 .. upperBound]

main :: IO ()
main = do
  xmonad . XMobar.render palette . ewmh . flip additionalKeys mKeys $
    docks
      def
        { manageHook = scratchpadHook <+> manageDocks <+> manageHook def,
          handleEventHook = dynamicPropertyChange "WM_NAME" spotifyFloatingHook,
          layoutHook = layout,
          startupHook = Background.spawn >> Compositor.spawn,
          workspaces = numWorkspaces 5,
          terminal = "kitty",
          borderWidth = fromIntegral borderWidthPx,
          focusedBorderColor = yellow,
          normalBorderColor = "#6986A0", -- TODO migrate
          modMask = mod4Mask
        }
  where
    palette@(Palette.Palette {Palette.yellow = yellow}) = Palette.defaultPalette
