module MyXMonad.Layout where

import XMonad                       (Full (..), (|||))
import XMonad.Hooks.ManageDocks     (avoidStruts)
import XMonad.Layout.Fullscreen     (fullscreenFull)
import XMonad.Layout.Gaps           (Direction2D (..), gaps)
import XMonad.Layout.NoBorders      (noBorders)
import XMonad.Layout.Renamed        (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile  (ResizableTall (..))
import XMonad.Layout.ThreeColumns   (ThreeCol (..))
import XMonad.Layout.WindowArranger (windowArrange)

-- Layout
layout = windowArrange (avoidStruts resize ||| full ||| avoidStruts threeCol)
  where
        -- Full
    full   = renamed [Replace "full"] $ noBorders (fullscreenFull Full)

    -- Tiled
    resize = renamed [Replace "tall"] $ ResizableTall 1 (2 / 100) (1 / 2) []

    -- Three Column Layout
    threeCol =
        renamed [Replace "3col"] $ ThreeCol 1         -- num windows to show initially
                                              (3 / 100) -- amount to resize while resizing
                                                        (1 / 3)   -- initial size of columns
