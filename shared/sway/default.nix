{
  lib,
  pkgs,
  ...
}:
{
  wayland.windowManager.sway =
    let
      ws1 = "1:browser";
      ws2 = "2:terminal";
      ws3 = "3:emacs";
      ws4 = "4:signal";
    in
    {
      enable = true;
      systemd.enable = true;
      checkConfig = false;
      extraConfig = "exec rm -f $WOBSOCK && mkfifo $WOBSOCK && tail -f $WOBSOCK | wob";
      config = rec {
        terminal = "alacritty";
        modifier = "Mod4";
        # Provided by swaybar
        bars = [ ];
        workspaceAutoBackAndForth = true;
        window = {
          commands = [
            {
              # Keeping VLC awake when I'm watching stuff
              command = "inhibit_idle focus";
              criteria = {
                instance = "vlc";
              };
            }
          ];
        };
        assigns = {
          "${ws1}" = [ { app_id = "firefox"; } ];
          "${ws2}" = [ { app_id = "Alacritty"; } ];
          "${ws3}" = [ { class = "Emacs"; } ];
          "${ws4}" = [ { class = "Signal"; } ];
        };
        # output = {
        #   "*" = {
        #     bg = "/home/lyterk/Pictures/selectedBackgrounds/background.png fill";
        #   };
        # };
        input = {
          "*" = {
            xkb_layout = "us(dvorak),es(dvorak)";
            xkb_options = "ctrl:nocaps,grp:rctrl_toggle";
          };
        };

        keybindings = lib.mkOptionDefault {
          "${modifier}+f2" = "exec ${pkgs.firefox}/bin/firefox";
          "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
          "${modifier}+p" = "exec ${../../shared/passmenu.sh}";
          "Shift+Print" =
            "exec ${pkgs.grim}/bin/grim ~/Pictures/screenshots/$(date +'%Y-%m-%d_%H-%M-%S_screenshot.png')";
          # Switch to workspace
          "${modifier}+1" = "workspace number ${ws1}";
          "${modifier}+2" = "workspace number ${ws2}";
          "${modifier}+3" = "workspace number ${ws3}";
          "${modifier}+4" = "workspace number ${ws4}";
          # Move container to workspace
          "${modifier}+Shift+1" = "move container to workspace number $ws1; workspace number ${ws1}";
          "${modifier}+Shift+2" = "move container to workspace number $ws2; workspace number ${ws2}";
          "${modifier}+Shift+3" = "move container to workspace number $ws3; workspace number ${ws3}";
          "${modifier}+Shift+4" = "move container to workspace number $ws4; workspace number ${ws4}";
          # Move container to HDMI device
          # Brightness
          "XF86MonBrightnessDown" = "exec light -U 10";
          "XF86MonBrightnessUp" = "exec light -A 10";
          # Loudness
          "XF86AudioMute" =
            "exec pactl set-sink-mute @DEFAULT_SINK@ toggle && pamixer --get-volume > $WOBSOCK";
          "XF86AudioRaiseVolume" =
            "exec pactl set-sink-volume @DEFAULT_SINK@ +5% && pamixer --get-volume > $WOBSOCK";
          "XF86AudioLowerVolume" =
            "exec pactl set-sink-volume @DEFAULT_SINK@ -5% && pamixer --get-volume > $WOBSOCK";
          # Personal mode
          "${modifier}+Shift+m" = "mode monitor";
          "${modifier}+m" = "mode kevin";
          "${modifier}+r" = "mode resize";
        };

        modes = {
          monitor = {
            "l" = "move container to output eDP-1; mode default";
            "m" = "move container to output HDMI-A-1; mode default";
          };
          kevin = {
            "c" = "exec ${pkgs.calibre}/bin/calibre; mode default";
            "g" = "exec ${pkgs.chromium}/bin/chromium; mode default";
            "e" = "exec ${pkgs.emacs30}/bin/emacsclient -c; mode default";
            "s" = "exec ${pkgs.signal-desktop}/bin/signal-desktop; mode default";
            "w" = "exec ~/dotfiles/scripts/rofi-wifi-menu.sh; mode default";
            "j" = "exec ${pkgs.rofi}/bin/rofi -modi 'emoji:rofimoji' -show emoji; mode default";
            "l" = "exec ${pkgs.gtklock}/bin/gtklock -d; mode default";
            "v" = "exec ${pkgs.vlc}/bin/vlc; mode default";
            "Escape" = "mode default";
            "Return" = "mode default";
          };
          resize = {
            Down = "resize grow height 10 px";
            Escape = "mode default";
            Left = "resize shrink width 10 px";
            Return = "mode default";
            Right = "resize grow width 10 px";
            Up = "resize shrink height 10 px";
            h = "resize shrink width 10 px";
            j = "resize grow height 10 px";
            k = "resize shrink height 10 px";
            l = "resize grow width 10 px";
          };
        };
      };
    };
}
