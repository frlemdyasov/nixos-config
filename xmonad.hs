import XMonad


main :: IO ()
main = xmonad $ myConfig

myConfig = def
    {  terminal    = "gnome-terminal"
    ,  modMask     = mod4Mask -- rebind Mod to Super Key
    ,  normalBorderColor = "white"
    ,  focusedBorderColor = "black"
    }


