module MyXMonad.Layout where

import XMonad (Full (..), (|||))
import XMonad.Hooks.ManageDocks (avoidStruts)
import           XMonad.Layout.ResizableTile      (ResizableTall (..))
import           XMonad.Layout.NoBorders          (noBorders)
import           XMonad.Layout.ThreeColumns       (ThreeCol (..))
import           XMonad.Layout.Fullscreen         (fullscreenFull)
import           XMonad.Layout.Gaps               (Direction2D (..), gaps)

resize = ResizableTall 1 (2 / 100) (1 / 2) []

full = noBorders (fullscreenFull Full)

threeCol =
  ThreeCol 1         -- num windows to show initially
           (3 / 100) -- amount to resize while resizing
           (1 / 3)   -- initial size of columns

layout = resize ||| full ||| threeCol
