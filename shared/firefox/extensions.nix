{
  config,
  lib,
  pkgs,
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
    (extensionFn "ublock-origin" "uBlock0@raymondhill.net" "navbar")
    (extensionFn "videospeed" "{7be2ba16-0f1e-4d93-9ebc-5164397477a9}" "navbar")
    (extensionFn "tridactyl-vim" "tridactyl.vim@cmcaine.co.uk" "navbar")
    (extensionFn "tree-style-tab" "treestyletab@piro.sakura.ne.jp" "navbar")
    (extensionFn "old-reddit-redirect" "{9063c2e9-e07c-4c2c-9646-cfe7ca8d0498}" "navbar")
    (extensionFn "multi-account-containers" "@testpilot-containers" "navbar")
  ];
in
{
  ExtensionSettings = myExtensions;
}
