import           Data.List                        (isInfixOf)
import           Graphics.X11.ExtraTypes.XF86     (xF86XK_AudioLowerVolume,
                                                   xF86XK_AudioMute,
                                                   xF86XK_AudioNext,
                                                   xF86XK_AudioPlay,
                                                   xF86XK_AudioPrev,
                                                   xF86XK_AudioRaiseVolume,
                                                   xF86XK_MonBrightnessDown,
                                                   xF86XK_MonBrightnessUp,
                                                   xF86XK_Search)
import           System.IO                        (Handle, hPutStrLn)
import           XMonad                           (Layout (..),
                                                   WorkspaceId, X, XConfig (..),
                                                   className, controlMask, def,
                                                   gets, mod4Mask, sendMessage,
                                                   shiftMask, title, windowset,
                                                   xK_Print, xK_h, xK_l, xK_n,
                                                   xK_p, xK_q, xK_z, xmonad,
                                                   (-->), (.|.), (<&&>), (<+>),
                                                   (=?))
import           XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext,
                                                   shiftToPrev)
import           XMonad.Actions.DynamicWorkspaces (appendWorkspace)
import           XMonad.Actions.SpawnOn           (spawnHere)
import           XMonad.Hooks.DynamicLog          (PP (..), dynamicLogWithPP,
                                                   dzenColor, pad)
import           XMonad.Hooks.EwmhDesktops        (ewmh)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks,
                                                   manageDocks)
import           XMonad.Hooks.ManageHelpers       (doRectFloat)
import           XMonad.StackSet                  (RationalRect (..))
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig             (additionalKeys)
import           XMonad.Util.Run                  (spawnPipe)
import           XMonad.Util.SpawnOnce            (spawnOnce)

import MyXMonad.Layout


-- colors
normalBorder = "#6986a0"
focusedBorder = "#ffca28" -- amber
foreground = "#DEE3E0"
background = "#263238" -- for material
winType = "#649A54" -- green

backgroundImage = "$HOME/Documents/background/*"

statusFont = "'Ubuntu Mono derivative Powerline:size=14'"

borderWidthPx = 3 :: Int

-- keys
mKeys =
  [ ((modm, xK_p)                 , spawnHere rofi)
  , ((modm .|. controlMask, xK_q) , spawnHere shutdown)
  , ((modm .|. shiftMask, xK_h)   , shiftToPrev >> prevWS)
  , ((modm .|. shiftMask, xK_l)   , shiftToNext >> nextWS)
  , ((modm .|. controlMask, xK_h) , prevWS)
  , ((modm .|. controlMask, xK_l) , nextWS)
  , ((modm .|. shiftMask, xK_n)   , newWorkspace)
  , ((0, xF86XK_AudioMute)        , spawnHere $ pamixer Mute)
  , ((0, xF86XK_AudioLowerVolume) , spawnHere $ pamixer Decrease)
  , ((0, xF86XK_AudioRaiseVolume) , spawnHere $ pamixer Increase)
  , ((0, xF86XK_AudioPlay)        , spawnHere $ playerctl Toggle)
  , ((0, xF86XK_AudioNext)        , spawnHere $ playerctl Next)
  , ((0, xF86XK_AudioPrev)        , spawnHere $ playerctl Previous)
  , ((0, xF86XK_Search)           , spawnHere rofi)
  , ((0, xK_Print)                , spawnHere $ scrot "")
  , ((modm, xK_Print)             , spawnHere $ scrot "-u")
  , ((0, xF86XK_MonBrightnessUp)  , spawnHere $ xbacklight Brighten)
  , ((0, xF86XK_MonBrightnessDown), spawnHere $ xbacklight Darken)
  ]
 where
  modm = mod4Mask
  scrot opts =
    "scrot "
      <> opts
      <> " -z '%Y-%m-%d-%H-%M-%s_screenshot.png' -e 'mv $f ~/Desktop/'"
  rofi = "fish -c 'rofi -show combi -modi combi'"
  shutdown
    = "dbus-send --system --print-reply --dest=org.freedesktop.login1 \
          \/org/freedesktop/login1 \
          \ \"org.freedesktop.login1.Manager.PowerOff\" \
          \ boolean:true"

data BacklightControl = Darken | Brighten

xbacklight :: BacklightControl -> String
xbacklight state =
  let amount = "5%"
  in
    "xbacklight " <> case state of
      Darken   -> "-dec " <> amount
      Brighten -> "-inc " <> amount

data VolumeControl = Increase | Decrease | Mute

pamixer :: VolumeControl -> String
pamixer state =
  let amount = "1"
  in
    "pamixer " <> case state of
      Increase -> "--increase " <> amount
      Decrease -> "--decrease " <> amount
      Mute     -> "-t"

data MediaControl = Toggle | Next | Previous

playerctl :: MediaControl -> String
playerctl state = "playerctl " <> case state of
  Toggle   -> "play-pause"
  Next     -> "next"
  Previous -> "previous"

logbar h = dynamicLogWithPP $ def
  { ppOutput          = hPutStrLn h
  , ppCurrent         = dzenColor foreground normalBorder
  , ppVisible         = dzenColor foreground background
  , ppHidden          = dzenColor foreground background
  , ppHiddenNoWindows = dzenColor foreground background
  , ppUrgent          = dzenColor foreground focusedBorder
  , ppOrder           = \(ws : l : _) -> [ws, l]
  , ppSep             = ""
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

-- | Get all workspaces.
allWs :: X [WorkspaceId]
allWs = do
  ws <- gets windowset
  return $ fmap W.tag $ (W.workspace . W.current $ ws) : W.hidden ws

-- | Create a new workspace at the end of the window list.
newWorkspace :: X ()
newWorkspace = do
  nextWs <- pad . show . succ . length <$> allWs
  appendWorkspace nextWs

myStartupHook =
  spawnHere ("feh --randomize --recursive --bg-fill " <> backgroundImage)
    >> spawnOnce "compton"
    >> spawnHere
         ("i3status | dzen2 -ta r -x 400 -h 48 -w 3440 -fn " ++ statusFont)

main :: IO ()
main = do
  bar <- spawnPipe ("dzen2 -ta l -p -w 400 -h 48 -fn " ++ statusFont)
  xmonad
    .                ewmh
    $                docks def
                       { manageHook         = manageDocks <+> manageHook def
                       , layoutHook         = layout
                       , startupHook        = myStartupHook
                       , workspaces         = numWorkspaces 3
                       , terminal           = "kitty"
                       , borderWidth        = fromIntegral borderWidthPx
                       , focusedBorderColor = focusedBorder
                       , normalBorderColor  = normalBorder
                       , logHook            = logbar bar
                       , modMask            = mod4Mask
                       }
    `additionalKeys` mKeys
