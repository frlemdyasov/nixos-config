# Introduction
This is my NixOS configuration that I'm using on my Lenovo Thinkpad T16 Gen 1.

This setup does not use home-manager or flakes to keep it simple.

# Non-Declarative Setup Instructions
- The Gnome extentions must be enabled using the extention manager.
- Steam needs to be logged into to update and install games.

# Unsolved Issues
- Right click on the Nautilus file manager doesn't show a terminal option.
  - This can be solved by: Right clicking the empty space -> Open with... -> Select Emacs -> Open -> M-x Term RET RET
- Can't declaratively install the nix-mode major mode for Emacs.
- Can't declaratively create another user.
