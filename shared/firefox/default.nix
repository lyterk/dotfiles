{
  config,
  lib,
  pkgs,
  ...
}:
let
  policies = (import ./policies.nix { inherit pkgs lib; });
  extensions = (import ./extensions.nix { inherit config lib pkgs; });
  bookmarksList = (import ./bookmarks.nix { inherit lib; });
in
{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    policies = lib.mkMerge [
      policies
      extensions
    ];
    profiles = {
      default = {
        id = 0;
        name = "kevinDefault";
        isDefault = true;
        bookmarks = {
          force = true;
          settings = bookmarksList;
        };
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.ai.control.default" = "blocked";
          "browser.ai.control.linkPreviewKeyPoints" = "blocked";
          "browser.ai.control.pdfjsAltText" = "blocked";
          "browser.ai.control.sidebarChatbot" = "blocked";
          "browser.ai.control.smartTabGroups" = "blocked";
          "browser.ai.control.translations" = "blocked";
        };
        userChrome = ''
          @namespace url(http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul);

          /* hides the native tabs */

          #TabsToolbar {
            visibility: collapse !important;
          }
        '';
      };
    };
  };
}
