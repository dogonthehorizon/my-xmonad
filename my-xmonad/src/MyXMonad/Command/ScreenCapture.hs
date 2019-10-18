module MyXMonad.Command.ScreenCapture where

data Region = Screen | Window

scrotOpts :: Region -> String
scrotOpts Screen = ""
scrotOpts Window = "-u"

screencap :: Region -> String
screencap region =
  "scrot "
    <> (scrotOpts region)
    <> " -z '%Y-%m-%d-%H-%M-%s_screenshot.png' -e 'mv $f ~/Desktop/'"
