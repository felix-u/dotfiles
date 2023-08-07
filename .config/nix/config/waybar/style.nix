let
  theme = import ../../system/theme.nix;
in
''
  * {
      border: none;
      border-radius: 0;
      font-family: "${theme.fontsans}", "FiraCode Nerd Font";
      font-weight: normal;
      box-shadow: inherit;
      text-shadow: inherit;
      min-height: 25px;
  }


  window#waybar {
      background: #${theme.cbg};
      color: #${theme.cfg};
  }

  #window {
      margin-top: 0px;
      margin-bottom: 6px;
      padding-left: 16px;
      padding-right: 16px;
      padding-top: 0;
      padding-bottom: 0;
      border-radius: 13px;
      transition: none;

  }

  #workspaces, #tags {
      margin-left: 8px;
      transition: none;
      background: transparent;
  }

  #workspaces button, #tags button {
      padding-left: 6px;
      padding-right: 6px;
      color: #${theme.c07};
  }

  #workspaces button:hover, #tags button:hover {
      background: transparent;
  }

  #tags button.occupied {
      color: #${theme.cfg};
  }

  #workspaces button.focused, #workspaces button.active, #tags button.focused {
      color: #${theme.cfg};
      text-shadow: 0px 0.5px, 0.5px 0px, 0.5px 0.5px;
      transition: none;
  }

  #cpu {
      color: transparent;
      margin-top: 7px;
      margin-bottom: 6px;
      margin-left: 6px;
      margin-right: 6px;
      padding: 0px 10px;
  }
  #cpu.warning {
      color: #${theme.c07};
  }
  #cpu.critical {
      color: #${theme.cfg};
  }

  #custom-clock {
      margin-top: 7px;
      margin-bottom: 6px;
      margin-right: 20px;
      margin-left: 0px;
      padding: 0px 9px;
      color: #${theme.cfg};
  }


  #custom-battery {
      margin-top: 7px;
      margin-bottom: 6px;
      margin-right: 6px;
      margin-left: 0px;
      padding: 0px 9px;
  }

  #battery {
      margin-top: 7px;
      margin-bottom: 6px;
      margin-right: 6px;
      margin-left: 0px;
      padding: 0px 9px;
      color: #${theme.cfg};
  }

  #pulseaudio, #pulseaudio.bluetooth {
      margin-top: 7px;
      margin-bottom: 6px;
      margin-left: 6px;
      margin-right: 6px;
      padding: 0px 10px;
      color: #${theme.c15};
  }

  #pulseaudio.muted, #pulseaudio.bluetooth.muted {
      color: #${theme.c07};
  }
''
