{
  lib,
  ...
}:

let
  mkGtklockCss = targetUrl: ''
    window {
      background-image: url("${targetUrl}");
      background-size: cover;
      background-repeat: no-repeat;
      background-position: center;
      background-color: gray;
      color: white;
    }

    /* Main container - align content to bottom-right */
    window box {
      margin: 50px;
    }

    #clock-label {
      font-size: 64px;
      color: #ffffff;
      font-weight: bold;
    }

    #date-label {
      font-size: 24px;
      color: #e0e0e0;
      margin-top: 5px;
      margin-bottom: 20px;
    }

    #input-field {
      background-color: rgba(0, 0, 0, 0.5);
      color: #ffffff;
      border: 2px solid #33ccff;
      border-radius: 8px;
      padding: 10px;
      font-size: 18px;
      min-width: 250px;
    }
  '';
in
{
  options.gtklock.mkGtklockCss = lib.mkOption {
    type = lib.types.raw;
    default = mkGtklockCss;
    readOnly = true;
    description = ''
      Return a CSS string for the login page, with the target url as the background image.
    '';
  };
}
