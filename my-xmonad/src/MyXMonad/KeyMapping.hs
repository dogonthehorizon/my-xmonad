module MyXMonad.KeyMapping where

import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioNext,
    xF86XK_AudioPlay,
    xF86XK_AudioPrev,
    xF86XK_AudioRaiseVolume,
    xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
    xF86XK_Search,
  )
import MyXMonad.Command.Backlight (BacklightControl (..), xbacklight)
import MyXMonad.Command.Media (MediaControl (..), VolumeControl (..), pamixer, playerctl)
import MyXMonad.Command.Notification (Notification (..), Severity (..), notification)
import MyXMonad.Command.ScreenCapture (Region (..), screencap)
import MyXMonad.Scratchpad (scratchpadUnprefixedKeyMap)
import XMonad
  ( WorkspaceId,
    X,
    controlMask,
    gets,
    mod4Mask,
    restart,
    shiftMask,
    windowset,
    xK_Print,
    xK_h,
    xK_l,
    xK_n,
    xK_p,
    xK_q,
    xK_r,
    xK_z,
    (.|.),
  )
import XMonad.Actions.CycleWS
  ( nextWS,
    prevWS,
    shiftToNext,
    shiftToPrev,
  )
import XMonad.Actions.DynamicWorkspaces (appendWorkspace)
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Hooks.DynamicLog (pad)
import qualified XMonad.StackSet as W

-- keys
mKeys =
  [ ((modm, xK_p), spawnHere rofi),
    ((modm .|. controlMask, xK_q), spawnHere shutdown),
    ((modm .|. shiftMask, xK_h), shiftToPrev >> prevWS),
    ((modm .|. shiftMask, xK_l), shiftToNext >> nextWS),
    -- Restart xmonad
    ((modm, xK_q), restart "/home/ffreire/.xmonad/xmonad-x86_64-linux" True),
    ((modm, xK_r), spawnHere "$HOME/bin/xmonad-recompile"),
    ((0, xF86XK_AudioMute), spawnHere $ pamixer Mute),
    ((0, xF86XK_AudioLowerVolume), spawnHere $ pamixer Decrease),
    ((0, xF86XK_AudioRaiseVolume), spawnHere $ pamixer Increase),
    ((0, xF86XK_AudioPlay), spawnHere $ playerctl Toggle),
    ((0, xF86XK_AudioNext), spawnHere $ playerctl Next),
    ((0, xF86XK_AudioPrev), spawnHere $ playerctl Previous),
    ((0, xK_Print), spawnHere $ screencap Screen),
    ((modm, xK_Print), spawnHere $ screencap Window),
    ((0, xF86XK_MonBrightnessUp), spawnHere $ xbacklight Brighten),
    ((0, xF86XK_MonBrightnessDown), spawnHere $ xbacklight Darken)
  ]
    ++ namedActions
  where
    modm = mod4Mask
    rofi = "fish -c 'rofi -show combi'"
    shutdown =
      "dbus-send --system --print-reply --dest=org.freedesktop.login1 \
      \/org/freedesktop/login1 \
      \ \"org.freedesktop.login1.Manager.PowerOff\" \
      \ boolean:true"
    namedActions =
      fmap
        (\(k, c) -> ((modm .|. controlMask, k), c))
        scratchpadUnprefixedKeyMap

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
