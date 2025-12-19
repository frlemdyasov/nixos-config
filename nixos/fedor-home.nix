{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchTarball https://github.com/nix-community/home-manager/archive/release-25.11.tar.gz;
in
{
  imports =
    [
      (import "${home-manager}/nixos")
    ];

#-------------------------------------------------------------------------------------------
# GNOME Desktop Configuration

  services.xserver = {
    enable = false; # Enable X11 windowing system
    xkb.layout = "us"; # Configure keymap in X11
    xkb.variant = "";
  };

  services.desktopManager.gnome.enable = true; # Enable GNOME desktop manager
  services.displayManager.gdm.enable = true; # Enable GNOME display manager

  # Uninstall most default GNOME programs
  services.gnome = {
    core-apps.enable = false;
    core-shell.enable = true;
    core-os-services.enable = true; # Essential for GNOME
    gnome-online-accounts.enable = false; # Disable online user accounts
    core-developer-tools.enable = false;
  };

  # Exclude default GNOME packages
  environment.gnome.excludePackages = (with pkgs; [
    gnome-tour		   # desktop tour
  ]);

  # Exclude default X11 packages
  services.xserver.excludePackages = [
    pkgs.xterm 		   # x11 terminal emulator
  ];

  # Enable GNOME default terminal
  programs.gnome-terminal.enable = true;
  
#-------------------------------------------------------------------------------------------
# System Services

  
  # Disable Bluetooth
  hardware.bluetooth.enable = false;
  
  # Install and set default fonts
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      dejavu_fonts
      overpass
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "DejaVu Serif" ];
        sansSerif = [ "DejaVu Sans" ];
        monospace = [ "DejaVu Sans Mono" ];
        #serif = [ "Overpass" ];
        #sansSerif = [ "Overpass" ];
        #monospace = [ "Overpass Mono" ];
      };
    };
  };

  # Enable emacs as a service/daemon
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacs-pgtk;
  };

  # Enable Guix Package Manager
  #services.guix.enable = true;
  
#-------------------------------------------------------------------------------------------
# Enabling Gstreamer Plugins

  environment.sessionVariables.GST_PLUGIN_SYSTEM_PATH_1_0 = lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (with pkgs.gst_all_1; [
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
    gst-libav
  ]);
  
#-------------------------------------------------------------------------------------------
# Virtual Machine Settings

  # Virtual Machine Options
  virtualisation = {
    libvirtd = {
      enable = true;  # Enable a virtualization dependency
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
      };
    };
  };

  programs.dconf.enable = true; # to remember virt-manager settings
  # A virtualization connection can be made in File->Add Connection
  
#-------------------------------------------------------------------------------------------
# Allow Non-Free Programs

  nixpkgs.config = {
    allowUnfree = false;  # Disallow non-free packages.
      # Before allowing non-free packages, please read: https://www.gnu.org/philosophy/free-sw.en.html
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [  # Make an exception for some non-free packages
      "steam"  # Digital game distribution platform, with DRM
      #"steam-original"  # Similar to the steam package, but is required for steam to function
      #"steam-run"  # Steam FHS envionment for commands
      "steam-unwrapped"
    ];
  };
  
#-------------------------------------------------------------------------------------------
# Program Configuration
  
    # Enable GPG encyption software
  programs.gnupg.agent = {
    enable = true;
  };

  # Enable and configure foot terminal
  programs.foot = {
    enable = true;
    #theme = "paper-color-light";
    settings = {
      main = {
        font = "DejaVu Sans Mono:size=11";
      };
      scrollback = {
        lines = 100000;
      };
      colors = {
        background = "ffffff";
        foreground = "444444";
        regular0 = "eeeeee";  # black
        regular1 = "a60000";  # red
        regular2 = "006800";  # green
        regular3 = "6f5500";  # yellow
        regular4 = "0031a9";  # blue
        regular5 = "721045";  # magenta
        regular6 = "005e8b";  # cyan
        regular7 = "a6a6a6";  # white
        bright0 = "bcbcbc";   # bright black
        bright1 = "d00000";   # bright red
        bright2 = "008900";   # bright green
        bright3 = "808000";   # bright yellow
        bright4 = "0000ff";   # bright blue
        bright5 = "dd22dd";   # bright magenta
        bright6 = "008899";   # bright cyan
        bright7 = "ffffff";   # bright white
      };
      csd = {
        font = "DejaVu Sans";
        size = 0; # 36
        color = "c8c8c8";
        button-width = 36;
        button-minimize-color = "deddda";
        button-maximize-color = "deddda";
        button-close-color =  "deddda";
        border-color = "deddda";
        border-width = 2;
      };
    };
  };

  # Enable ability to run unpatched dynamic binaries with nix-shell
  programs.nix-ld.enable = true;
  
    # Enable Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

#-------------------------------------------------------------------------------------------
# User Configuration

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    fedor = {
      isNormalUser = true;
      description = "Fedor Lemdyasov";
      extraGroups = [ "networkmanager" "wheel" "pipewire" ];
      packages = with pkgs; [	];
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    users.fedor = { pkgs, ... }: {
      home.packages = with pkgs; [
	      audacity				       # sound editor
	      borgbackup				     # file backup creator
	      calibre 				       # e-book tools
	      convertall				     # unit converter
	      dita-ot					       # publishing engine
	      eartag					       # audio tag editor
	      emacs-pgtk					   # best text editor
	      endless-sky				     # space trading game
	      fastfetch				       # computer stats
        ffmpegthumbnailer      # video thumbnailer
	      firefox 				       # main web browser
	      firejail				       # program sandboxer
	      flac					         # audio codec
        foot                   # terminal emulator
	      freac					         # audio converter
	      ghc					           # haskell compiler
	      gimp 					         # pixel image editor
	      hieroglyphic				   # latex symbol finder
	      inkscape 				       # vector image editor
	      jdk					           # java runtime
	      libreoffice				     # office suite
        lsix                   # sixel thumbnails
        mediainfo              # video audio tags
	      #metadata-cleaner 			 # file metadata eraser # Python test fails
	      mindustry				       # automation td game
	      nicotine-plus				   # p2p music downloader
	      obs-studio 				     # screen recorder
	      octaveFull 				     # programming calculator
        p7zip                  # archive tool
	      pass-wayland				   # password manager
	      picard 					       # music metadata
	      poppler-utils				   # pdf utilities
        prismlauncher          # minecraft launcher
	      qbittorrent 				   # torrent client
	      renameutils				     # file renamer
	      resources				       # task manager
	      shotcut					       # video editing
	      superTuxKart				   # racing game
	      stack					         # haskell toolkit
	      texliveFull				     # typesetting system
	      thunderbird 				   # email client
	      tor-browser 				   # privacy web browser
	      ungoogled-chromium 		 # compatability web browser
        vips                   # image processing system
	      virt-manager 				   # virtual machines
	      vlc					           # media player
	      whipper					       # cd ripper
	      xonotic					       # fps game
	      yt-dlp					       # yt video downloader
	      zotero					       # citation manager
        
	      # GNOME specific additions:

        adwaita-icon-theme		 # gnome icons
        baobab      	   			 # disk usage analyzer
        cheese      	  			 # photo booth
        eog         	   			 # image viewer
        evince      	   			 # document viewer
        file-roller 	   			 # archive manager
        glib					         # for gsettings
        gtk3.out				 	     # for gtk-launch program
        nautilus					     # file manager
        simple-scan 	   			 # document scanner
        xdg-user-dirs				   # default directories
        xdg-user-dirs-gtk 	   # default directory bookmarks
        
	      gnome-mines				                            # minesweeper game
	      gnomeExtensions.dash-to-panel 		            # desktop panel
        gnomeExtensions.just-perfection               # tweak tool
        gnomeExtensions.removable-drive-menu          # drive menu
        gnomeExtensions.rounded-window-corners-reborn # rounded windows
	      gnomeExtensions.thinkpad-battery-threshold    # battery saver
        
        #gnome-bluetooth				 # bluetooth
        gnome-calculator     	 # calculator
        gnome-control-center	 # gnome settings
        gnome-disk-utility   	 # disk manager
        gnome-maps           	 # map navigator
        gnome-screenshot     	 # screenshot utility
        gnome-tweaks			 	   # gtk3 settings
        gnome-user-docs				 # gnome documentation
      ];
      
      programs = {
        bash.enable = true;
        home-manager.enable = true;
      };
    # The state version is required and should stay at the version you
    # originally installed.
      home.stateVersion = "24.05";
      dconf.settings = {
        "org/gnome/shell" = {
          disable-user-extensions = false;
          disabled-extensions = "disabled";
          enabled-extensions = [
            "dash-to-panel@jderose9.github.com"
            "just-perfection-desktop@just-perfection"
            "drive-menu@gnome-shell-extensions.gcampax.github.com"
            "rounded-window-corners@fxgn"
            "thinkpad-battery-threshold@marcosdalvarez.org"
          ];
          favorite-apps = [
            "firefox.desktop"
            "thunderbird.desktop"
            "chromium-browser.desktop"
            "torbrowser.desktop"
            "emacs.desktop"
            "foot.desktop"
            "org.gnome.Nautilus.desktop"
            "virt-manager.desktop"
          ];
        };
        "org/gnome/settings-daemon/plugins/media-keys" = {
          custom-keybindings = [
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/"
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/"
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5/"
          ];
          help = "";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
          name = "Launch Emacs";
          command = "emacs";
          binding = "<Super><Shift>e";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
          name = "Launch Firefox";
          command = "firefox";
          binding = "<Super><Shift>f";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
          name = "Launch Nautilus";
          command = "nautilus -w";
          binding = "<Super><Shift>d";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
          name = "Launch Thunderbird";
          command = "thunderbird";
          binding = "<Super><Shift>r";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4" = {
          name = "Open NixOS Config using Emacs";
          command = "emacs --file /sudo::/etc/nixos/configuration.nix";
          binding = "<Super><Shift>c";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5" = {
          name = "Launch Foot";
          command = "foot";
          binding = "<Super><Shift>t";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom6" = {
          name = "Launch LinkedIn";
          command = "firefox --new-tab https://www.linkedin.com/";
          binding = "<Control><Shift><Super><Alt>l";
        };
        "org/gnome/shell/extensions/dash-to-panel" = {
          intellihide = true;
          intellihide-use-pointer = false;
          panel-positions = "{\"CMN-0x00000000\":\"TOP\"}";
          panel-element-positions = "{\"CMN-0x00000000\":[{\"element\":\"showAppsButton\",\"visible\":true,\"position\":\"stackedTL\"},{\"element\":\"activitiesButton\",\"visible\":false,\"position\":\"stackedBR\"},{\"element\":\"leftBox\",\"visible\":false,\"position\":\"stackedTL\"},{\"element\":\"taskbar\",\"visible\":true,\"position\":\"stackedTL\"},{\"element\":\"dateMenu\",\"visible\":true,\"position\":\"centerMonitor\"},{\"element\":\"centerBox\",\"visible\":false,\"position\":\"stackedBR\"},{\"element\":\"rightBox\",\"visible\":false,\"position\":\"stackedBR\"},{\"element\":\"systemMenu\",\"visible\":true,\"position\":\"stackedBR\"},{\"element\":\"desktopButton\",\"visible\":false,\"position\":\"stackedBR\"}]}";
          show-apps-icon-side-padding = 2;
          appicon-padding = 0;
          appicon-margin = 0;
          highlight-appicon-hover = true;
          dot-position = "TOP";
          dot-style-focused = "CILIORA";
          dot-style-unfocused = "DASHES";
          trans-use-custom-opacity = true;
          trans-panel-opacity = 0.0;
          show-window-previews = false;
          click-action = "LAUNCH";
          context-menu-entries = "[]";
          progress-show-count = false;
        };
        "org/gnome/shell/extensions/just-perfection" = {
           activities-button = false;
           clock-menu-position = 0;
           notification-banner-position = 1;
           osd = true;
           panel = false;
           panel-in-overview = true;
           panel-notification-icon = false;
           quick-settings-airplane-mode = false;
           quick-settings-dark-mode = false;
           quick-settings-night-light = false;
           search = false;
           support-notifier-type = 0;
           window-picker-icon = false;
           window-preview-close-button = true;
           workspaces-in-app-grid = true;
           workspace = false;
        };
        "org/gnome/shell/extensions/rounded-window-corners-reborn" = {
          focused-shadow = "{'verticalOffset': 4, 'horizontalOffset': 0, 'blurOffset': 28, 'spreadRadius': 2, 'opacity': 30}";
          unfocused-shadow = "{'verticalOffset': 2, 'horizontalOffset': 0, 'blurOffset': 12, 'spreadRadius': -1, 'opacity': 30}";
        };
        "org/gnome/shell/extensions/thinkpad-battery-threshold" = {
          color-mode = false;
          indicator-mode = "NEVER";
          show-current-values = true;
          show-notifications = false;
        };
        "org/gnome/desktop/interface" = {
          accent-color = "yellow";
          enable-animations = true;
          enable-hot-corners = false;
          toolkit-accessibility = false;
          document-font-name = "DejaVu Sans 11";
          font-name = "DejaVu Sans 11";
          monospace-font-name = "DejaVu Sans Mono 11";
          show-battery-percentage = true;
          gtk-enable-primary-paste = false;
          gtk-key-theme = "Emacs";
        };
        "org/gnome/desktop/wm/preferences" = {
          button-layout = "appmenu:minimize,maximize,close";
          num-workspaces = 2;    
        };
        "org/gnome/desktop/wm/keybindings" = {
          close = [ "<Super>q" ];
        };
        "org/gnome/desktop/peripherals/touchpad" = {
          tap-to-click = false;
        };
        "org/gnome/mutter" = {
          dynamic-workspaces = false;
        };
        "org/gtk/settings/file-chooser" = {
          clock-format = "12h";
        };
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };
      };
    };
  };
}
