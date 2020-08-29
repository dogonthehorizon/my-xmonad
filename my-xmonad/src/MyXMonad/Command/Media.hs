module MyXMonad.Command.Media where

data VolumeControl = Increase | Decrease | Mute

pamixer :: VolumeControl -> String
pamixer state =
    let amount = "1"
    in
        "pamixer " <> case state of
            Increase -> "--increase " <> amount
            Decrease -> "--decrease " <> amount
            Mute     -> "-t"

data MediaControl = Toggle | Next | Previous

playerctl :: MediaControl -> String
playerctl state = "playerctl " <> case state of
    Toggle   -> "play-pause"
    Next     -> "next"
    Previous -> "previous"

