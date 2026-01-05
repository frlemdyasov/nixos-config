# Edit configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:


{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./fedor-home.nix
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
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelParams = [ "snd-intel-dspcfg.dsp_driver=1" "i915.force_probe=46a6" ]; # This gets sound to work for Thinkpad T16 Gen 1
    kernelPackages = pkgs.linuxPackages_latest;
  };

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
    systemWide = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

    wireplumber.extraConfig.no-ucm = {
      "monitor.alsa.properties" = {
        "alsa.use-ucm" = false;
      };
    };
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Should fix sluggish touchpad? 5/15/25
  # services.libinput.enable = true;

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

#-------------------------------------------------------------------------------------------


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  
    coreutils-full		 # gnu core utulities
    git			     			 # git cloner
    wget		     			 # web retriever

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
