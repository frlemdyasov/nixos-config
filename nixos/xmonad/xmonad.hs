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
    { ppCurrent = xmobarColor "#008899" ""
    , ppHidden = xmobarColor "#005e8b" ""
    , ppHiddenNoWindows = xmobarColor "#a6a6a6" ""
    , ppTitle = xmobarColor "#5f5f5f" "" . shorten 50
    , ppLayout = xmobarColor "#5f5f5f" "" . shorten 50
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
  [ ("M-S-t", spawn "gnome-terminal")
  , ("M-S-e", spawn "emacs")
  , ("M-S-f", spawn "firefox")
  , ("M-d", spawn "dmenu_run")
  , ("M-q", kill)
  , ("<XF86AudioRaiseVolume>", spawn "amixer -D pipewire sset Master 3%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer -D pipewire sset Master 3%-")
  , ("<XF86AudioMute>", spawn "amixer -D pipewire sset Master 0%")
  ]

myConfig = def
    {  terminal    = "gnome-terminal"
    ,  modMask     = mod4Mask -- rebind Mod to Super Key
    ,  normalBorderColor = "#a6a6a6"
    ,  focusedBorderColor = "#bcbcbc"
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    }
    `additionalKeysP` myKeys

