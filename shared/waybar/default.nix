{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      mainBar = {
        position = "top";
        height = 24;
        modules-left = [
          "sway/workspaces"
          "sway/mode"
          "sway/scratchpad"
          "custom/media"
        ];
        modules-center = [ "sway/window" ];
        modules-right = [
          "idle_inhibitor"
          "temperature"
          "cpu"
          "memory"
          "network"
          "pulseaudio"
          "backlight"
          "keyboard-state"
          "battery"
          "sway/language"
          "tray"
          "clock"
        ];
        "sway/mode" = {
          "format" = "<span style=\"italic\">{}</span>";
        };
        "sway/language" = {
          "format" = "{short} {flag}";
        };
        "sway/scratchpad" = {
          "format" = "{icon} {count}";
          "show-empty" = false;
          "format-icons" = [
            ""
            ""
          ];
          "tooltip" = true;
          "tooltip-format" = "{app}: {title}";
        };
        "idle_inhibitor" = {
          "format" = "{icon}";
          format-icons = [
            ""
            ""
          ];
        };
        keyboard-state = {
          numlock = "true";
          capslock = "true";
          format = "{name} {icon}";
          format-icons = [
            ""
            ""
          ];
        };
        tray = {
          spacing = 10;
        };

        clock = {
          "tooltip-format" = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          "format" = "{:L%Y-%m-%d<small>[%a]</small> <tt><small>%p</small></tt>%I:%M}";
        };
        cpu = {
          "format" = " {usage}%";
        };
        memory = {
          "format" = " {}%";
        };
        temperature = {
          thermal-zone = 2;
          hwmon-path = "/sys/class/hwmon/hwmon1/temp1_input";
          critical-threshold = 80;
          format-critical = "{icon} {temperatureC}°C";
          format = "{icon} {temperatureC}°C";
          format-icons = [
            ""
            ""
            ""
          ];
        };
        backlight = {
          format = "{icon} {percent}%";
          format-icons = [
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
            ""
          ];
        };
        battery = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          format-alt = "{icon} {time}";
          format-icons = [
            ""
            ""
            ""
            ""
            ""
          ];
        };
        network = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = " {ifname}";
          tooltip-format = " {ifname} via {gwaddr}";
          format-linked = " {ifname} (No IP)";
          format-disconnected = "Disconnected ⚠ {ifname}";
          format-alt = " {ifname}: {ipaddr}/{cidr}";
        };
        pulseaudio = {
          scroll-step = 5; # %, can be a float
          format = "{icon} {volume}% {format_source}";
          format-bluetooth = " {icon} {volume}% {format_source}";
          format-bluetooth-muted = "  {icon} {format_source}";
          format-muted = "  {format_source}";
          format-source = " {volume}%";
          format-source-muted = "";
          format-icons = {
            default = [
              ""
              ""
              ""
            ];
          };
          on-click = "pavucontrol";
          on-click-right = "foot -a pw-top pw-top";
        };
      };
    };
    # style = ./style.css;
  };
}
