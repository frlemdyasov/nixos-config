{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchTarball https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz;
in
{
  imports =
    [
      (import "${home-manager}/nixos")
    ];

#-------------------------------------------------------------------------------------------
# NixOS Configuration

  nix = {
      package = pkgs.nix;
      settings.experimental-features = [ "nix-command" "flakes" ];
    };

  
#-------------------------------------------------------------------------------------------
# GNOME Desktop Configuration
  
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
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "DejaVu Serif" ];
        sansSerif = [ "DejaVu Sans" ];
        monospace = [ "DejaVu Sans Mono" ];
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
    settings = {
      main = {
        font = "DejaVu Sans Mono:size=11";
        initial-color-theme = "light";
      };
      scrollback = {
        lines = 100000;
      };
      colors-light = {
        background = "f2f2f2";
        foreground = "444444";
        regular0 = "000000";  # black
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

  # Thunar file manager configuration
  programs.thunar = {
    enable = true;
    plugins = with pkgs; [
      thunar-archive-plugin
      thunar-volman
    ];
  };
  programs.xfconf.enable = true; # Save changes made to Thunar preferences
  services.tumbler.enable = true; # Thumbnail support for images
  services.gvfs.enable = true; # Mount, trash, and other functionalities

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

#-------------------------------------------------------------------------------------------
# Installed Programs
  
  home-manager = {
    useGlobalPkgs = true;
    users.fedor = { pkgs, ... }: {
      home.packages = with pkgs; [
        alsa-utils              # sound utils
        audacity				        # sound editor
        beets                   # music tagger
        borgbackup				      # file backup creator
        bottles                 # wine manager
        calibre 				        # e-book tools
        ccls                    # c language server
        convertall				      # unit converter
        dita-ot					        # publishing engine
        eartag					        # audio tag editor
        emacs-pgtk		          # best text editor
        endless-sky				      # space trading game
        fastfetch				        # computer stats
        ffmpegthumbnailer       # video thumbnailer
        file-roller             # archive manager
        firefox 				        # main web browser
        flac					          # audio codec
        foot                    # wayland terminal emulator
        freac					          # audio converter
        gallery-dl              # gallery site downloader
        gcc                     # clang compiler
        gdb                     # debugger
        glade                   # gtk gui designer
        gimp 					          # pixel image editor
        go                      # programming language
        gopls                   # go lsp server
        hieroglyphic				    # latex symbol finder
        inkscape 				        # vector image editor
        jdk					            # java runtime
        libreoffice				      # office suite
        lsix                    # sixel thumbnails
        #metadata-cleaner 		   # file metadata eraser # Python test fails
        mediainfo               # video audio tags
        mindustry				        # automation td game
        nicotine-plus				    # p2p music downloader
        obs-studio 				      # screen recorder
        octaveFull 				      # programming calculator
        p7zip                   # archive tool
        pandoc                  # markup converter
        pass-wayland				    # password manager
        picard 					        # music metadata
        poppler-utils				    # pdf utilities
        prismlauncher           # minecraft launcher
        public-sans             # nice font
        qbittorrent 				    # torrent client
        renameutils				      # file renamer
        resources				        # task manager
        shotcut					        # video editing
        stack					          # haskell toolkit
        supertuxkart				    # racing game
        thunar                  # file manager
        texliveFull				      # typesetting system
        thunderbird 				    # email client
        tor-browser 				    # privacy web browser
        typescript              # javascript builder
        typescript-language-server # javascript language server
        ungoogled-chromium 		  # compatability web browser
        unzip                   # extraction utility
        vips                    # image processing system
        virt-manager 				    # virtual machines
        vlc					            # media player
        whipper					        # cd ripper
        xonotic					        # fps game
        yt-dlp					        # yt video downloader
        zotero                  # citation manager
        zip                     # archive tool
        
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
        
        gnomeExtensions.appindicator                  # panel indicator
        gnomeExtensions.just-perfection               # tweak tool
        gnomeExtensions.removable-drive-menu          # drive menu
        gnomeExtensions.rounded-window-corners-reborn # rounded windows
	      gnomeExtensions.thinkpad-battery-threshold    # battery saver
        gnomeExtensions.window-list                   # window list
        
        #gnome-bluetooth				 # bluetooth
        gnome-calculator     	 # calculator
        gnome-connections      # remote desktop client
        gnome-control-center	 # gnome settings
        gnome-disk-utility   	 # disk manager
        gnome-maps           	 # map navigator
        gnome-mines				     # minesweeper game
        gnome-screenshot     	 # screenshot utility
        gnome-tweaks			 	   # gtk3 settings
        gnome-user-docs				 # gnome documentation
      ];
      
      programs = {
        bash = {
          enable = true;
          shellAliases = {
            vim = "emacs -nw -q";
            upgrade = "sudo nixos-rebuild switch --upgrade";
          };
        };
        home-manager.enable = true;
      };
    # The state version is required and should stay at the version you
    # originally installed.
      home.stateVersion = "24.05";



#-------------------------------------------------------------------------------------------
# GNOME configuration
      
      dconf.settings = {
        "org/gnome/shell" = {
          disable-user-extensions = false;
          disabled-extensions = "window-list@gnome-shell-extensions.gcampax.github.com"; #"disabled";
          enabled-extensions = [
            "appindicatorsupport@rgcjonas.gmail.com"
            "just-perfection-desktop@just-perfection"
            "drive-menu@gnome-shell-extensions.gcampax.github.com"
            "rounded-window-corners@fxgn"
            "thinkpad-battery-threshold@marcosdalvarez.org"
            # "window-list@gnome-shell-extensions.gcampax.github.com"
          ];
          favorite-apps = [
            "firefox.desktop"
            "thunderbird.desktop"
            "chromium-browser.desktop"
            "torbrowser.desktop"
            "emacs.desktop"
            "foot.desktop"
            #"org.gnome.Nautilus.desktop"
            "thunar.desktop"
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
            "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom6/"
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
          name = "Launch Thunar";
          command = "thunar -w";
          binding = "<Super><Shift>d";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
          name = "Launch Thunderbird";
          command = "thunderbird";
          binding = "<Super><Shift>t";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4" = {
          name = "Open NixOS Config using Emacs";
          command = "emacs --file /sudo::/etc/nixos/configuration.nix";
          binding = "<Super><Shift>c";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5" = {
          name = "Launch Foot";
          command = "foot";
          binding = "<Super><Shift>Return";
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom6" = {
          name = "Launch LinkedIn";
          command = "firefox --new-tab https://www.linkedin.com/";
          binding = "<Control><Shift><Super><Alt>l";
        };
        "org/gnome/shell/extensions/just-perfection" = {
           activities-button = true;
           clock-menu-position = 0;
           notification-banner-position = 1;
           osd = true;
           top-panel-position = 0;
           panel = true;
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
          document-font-name = "Public Sans 11";
          font-name = "Public Sans 11";
          monospace-font-name = "DejaVu Sans Mono 11";
          show-battery-percentage = true;
          gtk-enable-primary-paste = false;
          gtk-key-theme = "Emacs";
          clock-format = "12h";
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
        "org/gnome/desktop/peripherals/mouse" = {
          accel-profile = "flat";
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
