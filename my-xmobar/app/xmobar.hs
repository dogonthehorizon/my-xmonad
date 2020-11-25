module Main where

import Xmobar (Config (..), Date (..), Monitors (..), Runnable (Run),
               StdinReader (..), XPosition (Top), defaultConfig, xmobar)

config :: Config
config = defaultConfig
    {
    -- Appearance
      font             =
        "xft:Ubuntu Mono derivative Powerline:size=14:antialias=true:hinting=true"
    , bgColor          = "#263238"
    , fgColor          = "#DEE3E0"
    , position         = Top
    , iconRoot         = "/home/ffreire/git/my-xmonad/icons"

    -- Layout
    , sepChar          = "%"
    , alignSep         = "}{"
    , template         =
        "%UnsafeStdinReader% }{ %default:Master% | %wlp2s0wi% | %battery% | %date%"
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
            , "#dc322f"
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
            "wlp2s0"
            [ "--template"
            , "<icon=connected.xpm/> <essid> <quality>"
            , "--high"
            , "#859900"
            , "--normal"
            , "#b58900"
            , "--low"
            , "#dc322f"
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
main = xmobar config
