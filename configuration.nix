# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:


{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.channel.enable = true;

  nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    "nixos-config=/etc/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = false;

#-------------------------------------------------------------------------------------------
# Bootloader

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [ "snd-intel-dspcfg.dsp_driver=1" "i915.force_probe=46a6" ]; # This gets sound to work for Thinkpad T16 Gen 1

#-------------------------------------------------------------------------------------------
# Networking

  networking.hostName = "metal-pole"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

#-------------------------------------------------------------------------------------------
# Localization

  # Set your time zone.
  #time.timeZone = "America/New_York";
  time.timeZone = "America/Denver";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

#-------------------------------------------------------------------------------------------
# NixOS Configuration

  nix.settings.experimental-features = [ "nix-command" ];
  # Experimental features should only be used to give compatability
  # with imperatively installed programs, or general workarounds.
  # No core system configuration should be experimental.

#-------------------------------------------------------------------------------------------
# System Services

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };
  # hardware.enableAllFirmware = true;

  # Should fix sluggish touchpad? 5/15/25
  # services.libinput.enable = true;

  # Disable Bluetooth
  hardware.bluetooth.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Fwupd firmware updating tool
  services.fwupd.enable = true;
  # Use the following sequence of commands:
  # fwupdmgr get-devices
  # fwupdmgr refresh
  # fwupdmgr get-updates
  # fwupdmgr update
  #
  # If \boot runs out of space, delete old linux generations:
  # sudo nix-collect-garbage  --delete-old
  # sudo /run/current-system/bin/switch-to-configuration boot

  services.xserver.videoDrivers = [ "modesetting" ];

  # Configure Intel Graphics (Xe iGPU)
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver     # VA-API (iHD) userspace
      vpl-gpu-rt             # oneVPL (QSV) runtime
      intel-compute-runtime  # OpenCL (NEO) + Level Zero for Arc/Xe
    ];
  };

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
        #monospace = [ "Overpass mono" ];
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
# Xmonad Configuration  

  # Enable xmonad
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = builtins.readFile /home/fedor/.xmonad/xmonad.hs;
  };

  # Contents of xmonad.hs
#  import XMonad
#
#  main :: IO ()
#  main = xmonad $ def
#      {  terminal    = "gnome-terminal"
#      ,  modMask     = mod4Mask -- rebind Mod to Super Key
#      ,  normalBorderColor = "white"
#      ,  focusedBorderColor = "black"
#      }

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
  

  
#-------------------------------------------------------------------------------------------
# GNOME Desktop Configuration

  services.xserver = {
    enable = true; # Enable X11 windowing system
    displayManager.gdm.enable = true; # Enable GNOME display manager
    desktopManager.gnome.enable = true; # Enable GNOME desktop manager
    xkb.layout = "us"; # Configure keymap in X11
    xkb.variant = "";
  };

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

  # Enable Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

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

  # How to fix no internet in VMs 4/19/25
  # In general: https://blog.programster.org/kvm-missing-default-network
  # In windows: https://superuser.com/questions/1671932/unable-to-connect-to-internet-in-windows-10-vm-using-kvm-qemu

#-------------------------------------------------------------------------------------------
# Package Lists

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    fedor = {
      isNormalUser = true;
      description = "Fedor Lemdyasov";
      extraGroups = [ "networkmanager" "wheel" ];
      packages = with pkgs; [

	audacity				       # sound editor
	borgbackup				     # file backup creator
	calibre 				       # e-book tools
	convertall				     # unit converter
	dita-ot					       # publishing engine
	eartag					       # audio tag editor
	emacs-pgtk					     # best text editor
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
	poppler_utils				   # pdf utilities
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

	gnome-mines				                            # minesweeper game
	#gnomeExtensions.dash-to-panel 		            # desktop panel
  gnomeExtensions.just-perfection               # tweak tool
  gnomeExtensions.rounded-window-corners-reborn # rounded windows
	gnomeExtensions.thinkpad-battery-threshold    # battery saver
  gnomeExtensions.unite                         # layout tweaks
	
      ];
    };
  };

#-------------------------------------------------------------------------------------------
# Enabling Gstreamer Plugins

  environment.sessionVariables.GST_PLUGIN_SYSTEM_PATH_1_0 = lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (with pkgs.gst_all_1; [
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
    gst-libav
  ]);

#-------------------------------------------------------------------------------------------


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  
    coreutils-full		 # gnu core utulities
    git			     			 # git cloner
    wget		     			 # web retriever

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

    #gnome-bluetooth				 # bluetooth
    gnome-calculator     	 # calculator
    gnome-control-center	 # gnome settings
    gnome-disk-utility   	 # disk manager
    gnome-maps           	 # map navigator
    gnome-screenshot     	 # screenshot utility
    gnome-tweaks			 	   # gtk3 settings
    gnome-user-docs				 # gnome documentation


# Commented out because of non-use 2/22/25
#    # Create an Filesystem Hierarchy Standard (FHS) environment using the command `fhs`
#    # This enables the execution of non-nixos packaged binaries
#    (let base = pkgs.appimageTools.defaultFhsEnvArgs; in
#      pkgs.buildFHSUserEnv (base // {
#      name = "fhs";
#        # pkgs.buildFHSUserEnv provides only a minimal FHS environment.
#        # Therefore, we need to add basic packages manually.
#        # pkgs.appimageTools provides basic packages required by most software.
#      targetPkgs = pkgs: (base.targetPkgs pkgs) ++ [
#        pkgs.pkg-config
#        # Packages are added here	
#      ]; 
#      profile = "export FHS=1";
#      runScript = "bash";
#      extraOutputsToInstall = ["dev"];
#    }))
#    # https://nixos-and-flakes.thiscute.world/best-practices/run-downloaded-binaries-on-nixos
  ];

#-------------------------------------------------------------------------------------------
# Do Not Touch

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
