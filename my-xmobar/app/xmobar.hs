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
        [ "Font Awesome 6 Free Solid 22",
          "Font Awesome 6 Brands 22"
        ],
      bgColor = Palette.backgroundColor palette,
      fgColor = Palette.foregroundColor palette,
      position = TopSize C 100 65,
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
                "2",
                "--low",
                Palette.red palette,
                "-f",
                "<fn=1>\xf244\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf241\xf240</fn>",
                "--",
                "--on",
                "<fn=1> \xf1e6</fn>", -- Battery charging progress
                "--idle",
                "<fn=1> \xf14a</fn>", -- Battery charging done
                "--off",
                "<fn=1> \xf240</fn>" -- Battery mid
              ]
              50,
          -- There's something of an IconPattern class we can use to
          -- potentially provide different icons for signal strength.
          Run $
            Wireless
              "wlp0s20f3"
              [ "--template",
                " <fn=1>\xf1eb</fn> <essid> <quality>",
                "--high",
                Palette.green palette,
                "--normal",
                Palette.yellow palette,
                "--low",
                Palette.red palette
              ]
              30,
          Run $ Date " %R %a %Y-%m-%d " "date" 1,
          Run $
            Volume
              "default"
              "Master"
              [ "--template",
                "<status> <volume>%",
                "--",
                "-O",
                "<fn=1>\xf028</fn>",
                "-o",
                "<fn=1>\xf6a9</fn>"
              ]
              10
        ]
    }

main :: IO ()
main = xmobar (config Palette.defaultPalette)
