module MyXMonad.Command.Notification where

data Severity = Normal | Low | Critical

instance Show Severity where
  show Normal = "normal"
  show Low = "low"
  show Critical = "critical"

type Summary = String

type Body = String

data Notification = Notification Summary Body (Maybe Severity)

notification :: Notification -> String
notification (Notification s b Nothing) = notification $ Notification s b (Just Low)
notification (Notification s b (Just sev)) =
  "notify-send --urgency \"" <> show sev <> "\" --app-name \"XMonad\" \"" <> s <> "\" \"" <> b <> "\" "
