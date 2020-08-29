module MyXMonad.Command.Backlight where

data BacklightControl = Darken | Brighten

xbacklight :: BacklightControl -> String
xbacklight state =
    let amount = "5%"
    in
        "xbacklight " <> case state of
            Darken   -> "-dec " <> amount
            Brighten -> "-inc " <> amount
