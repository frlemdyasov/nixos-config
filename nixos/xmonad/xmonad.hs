import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

main :: IO ()
main = xmonad $ withEasySB myStatusBar defToggleStrutsKey $ myConfig

myLayoutHook = avoidStruts $ layoutHook def

myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent = xmobarColor "#e66100" ""
    , ppHidden = xmobarColor "#62a0ea" ""
    , ppHiddenNoWindows = xmobarColor "#a6a6a6" ""
    , ppTitle = xmobarColor "#f6f5f4" "" . shorten 50
    , ppLayout = xmobarColor "#f6f5f4" "" . shorten 50
    , ppSep = " "
    , ppWsSep = " "
    }

myStatusBar = statusBarProp "xmobar /etc/nixos/xmonad/xmobarrc" (pure myXmobarPP)

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myKeys =
  [ ("M-S-<Return>", spawn "gnome-terminal")
  , ("M-S-t", spawn "thunderbird")
  , ("M-S-e", spawn "emacsclient -c")
  , ("M-S-f", spawn "firefox")
  , ("M-S-p", spawn "firefox --private-window")
  , ("M-d", spawn "dmenu_run")
  , ("M-q", kill)
  , ("<Print>", spawn "maim -s ~/Pictures/Screenshots/$(date +%Y%m%d-%H%M%S)-screenshot.png")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -D pipewire sset Master 3%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer -D pipewire sset Master 3%-")
  , ("<XF86AudioMute>", spawn "amixer -D pipewire sset Master 0%")
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 5%+ -d intel_backlight")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%- -d intel_backlight")
  ]

myConfig = def
    {  terminal    = "gnome-terminal"
    ,  modMask     = mod4Mask -- rebind Mod to Super Key
    ,  normalBorderColor = "#a6a6a6"
    ,  focusedBorderColor = "#bcbcbc"
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , focusFollowsMouse = False
    , borderWidth = 0
    }
    `additionalKeysP` myKeys

