module MyXMonad.KeyMapping where

import           XMonad.Actions.SpawnOn           (spawnHere)
import           XMonad.Actions.DynamicWorkspaces (appendWorkspace)
import           XMonad                           (WorkspaceId, X, controlMask,
                                                   mod4Mask,
                                                   shiftMask,
                                                   xK_Print, xK_h, xK_l, xK_n,
                                                   xK_p, xK_q, xK_z,
                                                   gets, windowset,
                                                   (.|.))
import           XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext, shiftToPrev)
import           Graphics.X11.ExtraTypes.XF86     (xF86XK_AudioLowerVolume,
                                                   xF86XK_AudioMute,
                                                   xF86XK_AudioNext,
                                                   xF86XK_AudioPlay,
                                                   xF86XK_AudioPrev,
                                                   xF86XK_AudioRaiseVolume,
                                                   xF86XK_MonBrightnessDown,
                                                   xF86XK_MonBrightnessUp,
                                                   xF86XK_Search)
import           XMonad.Hooks.DynamicLog          (pad)
import qualified XMonad.StackSet                  as W


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

-- | Create a new workspace at the end of the window list.
newWorkspace :: X ()
newWorkspace = do
  nextWs <- pad . show . succ . length <$> allWs
  appendWorkspace nextWs

-- | Get all workspaces.
allWs :: X [WorkspaceId]
allWs = do
  ws <- gets windowset
  return $ fmap W.tag $ (W.workspace . W.current $ ws) : W.hidden ws
