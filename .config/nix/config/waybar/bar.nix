{ config, pkgs }:

{
  mainBar = {
    layer = "top";
    modules-left = [ "river/tags" "cpu" ];
    modules-right =
      if config.networking.hostName == "thonkpad" then
        [ "pulseaudio" "battery" "custom/clock" ]
      else
        [ "pulseaudio" "custom/clock" ];
    "river/tags" = {
      num-tags = 4;
    };
    cpu = {
      interval = 2;
      format = "CPU: {usage}%";
      max-length = 10;
      states = {
        warning = 50;
        critical = 80;
      };
    };
    battery = {
      format = "{icon}  {capacity}%";
      bat = "BAT0";
      format-charging = "󰂄  {capacity}%";
      format-icons = [ "󰂃" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂀" "󰂁" "󰂂" "󰁹" ];
      states = {
        warning = 35;
        critical = 1;
      };
    };
    "custom/clock" = {
      exec = pkgs.writeShellScript "waybar_date" ''
        echo "   $(date "+%a %d  %H:%M")"
      '';
      restart-interval = 5;
      tooltip = false;
    };
    "custom/battery" = {
      exec = pkgs.writeShellScript "waybar_battery" ''
        NUM="$(cat /sys/class/power_supply/BAT0/capacity | tr -d '\n')"
        STATE="$(cat /sys/class/power_supply/BAT0/status | tr -d '\n')"
        BAT=""
        if   [ "$STATE" = "Charging" ]; then
            BAT=""
        elif [ "$STATE" = "Discharging" ]; then
            if   [ "$NUM" -gt "95" ]; then
                BAT=""
            elif [ "$NUM" -gt "85" ]; then
                BAT=""
            elif [ "$NUM" -gt "70" ]; then
                BAT=""
            elif [ "$NUM" -gt "60" ]; then
                BAT=""
            elif [ "$NUM" -gt "50" ]; then
                BAT=""
            elif [ "$NUM" -gt "40" ]; then
                BAT=""
            elif [ "$NUM" -gt "30" ]; then
                BAT=""
            elif [ "$NUM" -gt "20" ]; then
                BAT=""
            else
                BAT=""
            fi
        fi
        printf "$BAT  $NUM%%"
      '';
      restart-interval = 5;
      tooltip = false;
    };
    pulseaudio = {
      format = "{icon}   {volume}%";
      format-icons = {
        headphone = [ " " " " " " " " ];
        default = [ "" "" "" "" ];
        bluetooth = [ " " " " " " " " ];
      };
      # format-muted = "   {volume}%;
    };
  };
}
