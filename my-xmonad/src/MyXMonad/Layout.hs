module MyXMonad.Layout where

import XMonad                       (Full (..), (|||))
import XMonad.Hooks.ManageDocks     (avoidStruts)
import XMonad.Layout.Fullscreen     (fullscreenFull)
import XMonad.Layout.Gaps           (Direction2D (..), gaps)
import XMonad.Layout.NoBorders      (noBorders)
import XMonad.Layout.ResizableTile  (ResizableTall (..))
import XMonad.Layout.ThreeColumns   (ThreeCol (..))
import XMonad.Layout.WindowArranger (windowArrange)

resize = ResizableTall 1 (2 / 100) (1 / 2) []

full = noBorders (fullscreenFull Full)

threeCol =
    ThreeCol 1         -- num windows to show initially
               (3 / 100) -- amount to resize while resizing
                         (1 / 3)   -- initial size of columns

withGaps l = gaps [(U, 48), (R, 0), (L, 0), (D, 0)] (avoidStruts l)

layout = windowArrange $ withGaps resize ||| full ||| withGaps threeCol
