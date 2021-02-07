module Main where

import           Data.Color.Palette (Palette, defaultPalette)
import qualified Data.Color.Palette as Palette
import           Xmobar             (Config (..), Date (..), Monitors (..),
                                     Runnable (Run), StdinReader (..),
                                     XPosition (Top), defaultConfig, xmobar)

config :: Palette -> Config
config palette = defaultConfig
    {
    -- Appearance
      font             =
        "xft:Ubuntu Mono derivative Powerline:size=14:antialias=true:hinting=true"
    , bgColor          = Palette.backgroundColor palette
    , fgColor          = Palette.foregroundColor palette
    , position         = Top
    , iconRoot         = "/home/ffreire/git/my-xmonad/icons"

    -- Layout
    , sepChar          = "%"
    , alignSep         = "}{"
    , template         =
        "%UnsafeStdinReader% }{ %default:Master% | %wlp0s20f3wi% | %battery% | %date%"
    , lowerOnStart     = True
    , hideOnStart      = False
    , allDesktops      = True
    , overrideRedirect = True
    , pickBroadest     = False
    , persistent       = True

    -- Command Config
    , commands         =
        [ Run $ Battery
            [ "--template"
            , "<acstatus> <left>% (<timeleft>)"
            , "--Low"
            , "30"
            , "--High"
            , "80"
            , "-p"
            , "3"
            , "--low"
            , Palette.red palette
            , "--"
            , "-O"
            , "<icon=battery-mid-charging.xpm/>"
            , "-i"
            , "<icon=battery-charging.xpm/>"
            , "-o"
            , "<icon=battery-mid.xpm/>"
            ]
            50
        , Run UnsafeStdinReader
        , Run $ Wireless
            "wlp0s20f3"
            [ "--template"
            , "<icon=connected.xpm/> <essid> <quality>"
            , "--high"
            , Palette.green palette
            , "--normal"
            , Palette.yellow palette
            , "--low"
            , Palette.red palette
            ]
            30
        , Run $ Date "<icon=calendar.xpm/> %R %a %Y-%m-%d " "date" 1
        , Run $ Volume
            "default"
            "Master"
            [ "--template"
            , "<status> <volume>%"
            , "--"
            , "-O"
            , "<icon=volume-on.xpm/>"
            , "-o"
            , "<icon=volume-muted.xpm/>"
            ]
            10
        ]
    }

main :: IO ()
main = xmobar (config defaultPalette)
