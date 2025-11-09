# Introduction
This is my NixOS configuration that I'm using on my Lenovo Thinkpad T16 Gen 1.

This setup does not use home-manager or flakes to keep it simple.

# Non-Declarative Setup Instructions
- Gnome settings/tweaks need to be manually set.
- The Gnome extentions must be enabled using the extention manager.
- Steam needs to be logged into to update and install games.
- Thunderbird email accounts need to be manually added
- Emacs packages need to be installed using M-x package-install.
- Create a ~/.xmonad/xmonad.hs file (contents are written in configuration.nix).

# Unsolved Issues
- Right click on the Nautilus file manager doesn't show a terminal option.
  - This can be solved by: Right clicking the empty space -> Open with... -> Select Emacs -> Open -> M-x Term RET RET
  - Or: Left clicking the three dots -> Copy Location -> Open Terminal -> Type: cd -> C-S-v RET
