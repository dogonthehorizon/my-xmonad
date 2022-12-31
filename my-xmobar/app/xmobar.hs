module Main where

import Data.Color.Palette (Palette, defaultPalette)
import qualified Data.Color.Palette as Palette
import Xmobar
  ( Align (C),
    Config (..),
    Date (..),
    Monitors (..),
    Runnable (Run),
    XMonadLog (XMonadLog),
    XPosition (TopSize),
    defaultConfig,
    xmobar,
  )

config :: Palette -> Config
config palette =
  defaultConfig
    { -- Appearance
      font = "Cantarell Bold 30",
      additionalFonts =
        [ "Mononoki 40",
          "Font Awesome 6 Free Solid 30",
          "Font Awesome 6 Brands 30",
          "Cantarell Bold 30"
        ],
      bgColor = Palette.backgroundColor palette,
      fgColor = Palette.foregroundColor palette,
      position = TopSize C 100 55,
      iconRoot = "/home/ffreire/git/my-xmonad/icons",
      -- Layout
      sepChar = "%",
      alignSep = "}{",
      template =
        "  %XMonadLog% }{ %default:Master% | %wlp0s20f3wi% | %battery% | %date%  ",
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = True,
      commands =
        [ Run XMonadLog,
          Run $
            Battery
              [ "--template",
                "<acstatus> <left>% (<timeleft>)",
                "--Low",
                "30",
                "--High",
                "80",
                "-p",
                "3",
                "--low",
                Palette.red palette,
                "--",
                "-O",
                "<icon=battery-mid-charging.xpm/>",
                "-i",
                "<icon=battery-charging.xpm/>",
                "-o",
                "<icon=battery-mid.xpm/>"
              ]
              50,
          Run $
            Wireless
              "wlp0s20f3"
              [ "--template",
                "<icon=connected.xpm/> <essid> <quality>",
                "--high",
                Palette.green palette,
                "--normal",
                Palette.yellow palette,
                "--low",
                Palette.red palette
              ]
              30,
          Run $ Date "<icon=calendar.xpm/> %R %a %Y-%m-%d " "date" 1,
          Run $
            Volume
              "default"
              "Master"
              [ "--template",
                "<status> <volume>%",
                "--",
                "-O",
                "<icon=volume-on.xpm/>",
                "-o",
                "<icon=volume-muted.xpm/>"
              ]
              10
        ]
    }

main :: IO ()
main = xmobar (config Palette.defaultPalette)
