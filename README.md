# Introduction
This is my NixOS configuration that I'm using on my laptop.

I am using home-manager to declaratively configure gnome.

This setup does not use flakes to keep it simple.


# The Setup
- Version: NixOS 25.05
- Host: Lenovo ThinkPad T16 Gen 1
- Destop Environment: gnome (primary), xmonad (backup)
- Browser: firefox (primary), ungoogled-chromium (backup), tor (backup)
- Terminal: foot
- Editor: emacs-pgtk
- Email: Thunderbird

# Non-Declarative Setup Instructions
- Steam needs to be logged into to update and install games.
- Thunderbird email accounts need to be manually added
- Emacs needs to be configured by copying .emacs into ~/ .
- Xmonad needs to be configured by copying xmonad.hs into ~/.xmonad/ .
- The wallpaper must be set.
- Firefox extentions need to be installed
- Firefox settings need to be set
- Cursor needs to be set (Mocu-White-Right)
- Icons need to be organized

# Unsolved Issues
- Right click on the Nautilus file manager doesn't show a terminal option.
  - This can be solved by: Right clicking the empty space -> Open with... -> Select Emacs -> Open -> M-x Term RET RET
  - Or: Left clicking the three dots -> Copy Location -> Open Terminal -> Type: cd SPC -> C-S-v RET
