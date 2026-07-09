{ lib, ... }:
[
  {
    name = "Search Engines";
    toolbar = true;
    bookmarks = [
      {
        name = "Wikipedia";
        keyword = "w";
        url = "https://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go";
      }
      {
        name = "Wiktionary";
        keyword = "wi";
        url = "https://en.wiktionary.org/wiki/Special:Search?go=Go&search=%s&ns0=1";
      }
    ];
  }
  {
    name = "Nix";
    toolbar = true;
    bookmarks = [
      {
        name = "Home Manager";
        toolbar = false;
        bookmarks = [
          {
            name = "home-manager generated firefox add-ons";
            url = "https://github.com/nix-community/nur-combined/blob/main/repos/rycee/pkgs/firefox-addons/generated-firefox-addons.nix";
          }
          {
            name = "home-manager configuration options";
            url = "https://nix-community.github.io/home-manager/options.xhtml";
          }
        ];
      }
      {
        name = "Nixos Package Search";
        keyword = "np";
        url = "https://search.nixos.org/packages?channel=26.05&query=%s";
      }
    ];
  }
  {
    name = "News";
    toolbar = true;
    bookmarks = [
      {
        name = "Financial Times FT";
        url = "https://ft.com";
      }
      {
        name = "Seattle Times";
        url = "https://seattletimes.com";
      }
      {
        name = "New York Times NYT";
        url = "https://nyt.com";
      }
    ];
  }
]
