{pkgs, ...}: {
  programs.browserpass.enable = true;
  programs.firefox = {
    enable = true;
    profiles.misterio = {
      bookmarks = {};
      extensions = with pkgs.inputs.firefox-addons; [
        ublock-origin
        browserpass
      ];
      bookmarks = {};
      settings = {
        "browser.disableResetPrompt" = true;
        "browser.download.panel.shown" = true;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.startup.homepage" = "https://start.duckduckgo.com";
        "dom.security.https_only_mode" = true;
	"toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "identity.fxaccounts.enabled" = false;
        "privacy.trackingprotection.enabled" = true;
        "signon.rememberSignons" = false;
      };
    };
  };

  home = {
    persistence = {
      # Not persisting is safer
      # "/persist/home/misterio".directories = [ ".mozilla/firefox" ];
    };
  };

  xdg.mimeApps.defaultApplications = {
    "text/html" = ["firefox.desktop"];
    "text/xml" = ["firefox.desktop"];
    "x-scheme-handler/http" = ["firefox.desktop"];
    "x-scheme-handler/https" = ["firefox.desktop"];
  };
}
