module MyXMonad.Scratchpad where

import XMonad.Core (Query, ManageHook)
import XMonad ((=?), resource, xK_t, xK_c, xK_s)
import XMonad.Util.NamedScratchpad (customFloating, namedScratchpadAction,
                                    namedScratchpadManageHook,
                                    NamedScratchpad(NS))
import qualified XMonad.StackSet as W

class ToScratchpad a where
  toScratchpad  :: a -> (String, String, Query Bool, ManageHook)

data Scratchpad = Terminal | Telegram | Spotify
  deriving Eq

defaultFloatingHook =
  customFloating $ W.RationalRect l t w h
      where h = 0.9
            w = 0.9
            t = 0.95 -h
            l = 0.95 -w

instance Show Scratchpad where
  show Terminal = "terminal"
  show Telegram = "telegram"
  show Spotify  = "spotify"

instance ToScratchpad Scratchpad where
  toScratchpad Terminal =
    (show Terminal, "kitty --name=scratchpad", resource =? "scratchpad", defaultFloatingHook)
  -- TODO this window isn't floating
  toScratchpad Telegram =
    (show Telegram, "telegram-desktop", resource =? "telegram-desktop", defaultFloatingHook)
  -- TODO this window isn't floating
  toScratchpad Spotify =
    (show Spotify, "spotify", resource =? "spotify", defaultFloatingHook)

scratchpads = (\(n, c, f, h) -> NS n c f h) . toScratchpad <$>[Terminal, Telegram, Spotify]

scratchpadUnprefixedKeyMap = [
    (xK_t, namedScratchpadAction scratchpads (show Terminal)),
    (xK_c, namedScratchpadAction scratchpads (show Telegram)),
    (xK_s, namedScratchpadAction scratchpads (show Spotify))
  ]

scratchpadHook = namedScratchpadManageHook scratchpads
