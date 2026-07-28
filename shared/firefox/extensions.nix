{
  lib,
  ...
}:
let
  extensionFn = shortId: uuid: defaultArea: {
    name = uuid;
    value = {
      install_url = "https://addons.mozilla.org/en-US/firefox/downloads/latest/${shortId}/latest.xpi";
      installation_mode = "force_installed"; # normal_installed
      default_area = defaultArea;
      blocked_install_message = "Not allowed to install addons";
    };
  };
  myExtensions = lib.listToAttrs [
    # Note: Look in about:memory, 'Other Measurements'
    (extensionFn "ublock-origin" "uBlock0@raymondhill.net" "navbar")
    (extensionFn "videospeed" "{7be2ba16-0f1e-4d93-9ebc-5164397477a9}" "navbar")
    (extensionFn "tridactyl-vim" "tridactyl.vim@cmcaine.co.uk" "navbar")
    (extensionFn "tree-style-tab" "treestyletab@piro.sakura.ne.jp" "navbar")
    (extensionFn "old-reddit-redirect" "{9063c2e9-e07c-4c2c-9646-cfe7ca8d0498}" "navbar")
    (extensionFn "multi-account-containers" "@testpilot-containers" "navbar")
    (extensionFn "leechblock-ng" "leechblockng@proginosko.com" "navbar")
    (extensionFn "multiple-tab-handler" "multipletab@piro.sakura.ne.jp" "navbar")
    # Last 100 subreddits a user visited
    (extensionFn "neutral-reddit-masstagger" "{007e5327-f1ba-433d-aead-41cab2b7afb1}" "navbar")
    # (extensionFn "reveddit-real-time" "real-time-stable@reveddit.com" "navbar")
    (extensionFn "reddit-enhancement-suite" "jid1-xUfzOsOFlzSOXg@jetpack" "navbar")
    # Fix clickbait youtube thumbnails
    # (extensionFn "dearrow" "deArrow@ajay.app" "navbar")
  ];
in
{
  ExtensionSettings = myExtensions;
}
