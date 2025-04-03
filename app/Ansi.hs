module Ansi (red) where

ansi :: String -> String
ansi = ("\x1b" ++)

ansiEnd :: String
ansiEnd = ansi "[0m"

red :: String -> String
red text = ansi "[31m" ++ text ++ ansiEnd
