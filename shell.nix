let
  pkgs = import <nixpkgs> { config = {}; overlays = []; };
in
  pkgs.mkShellNoCC {
    packages = with pkgs; [
      elmPackages.elm
      elmPackages.elm-language-server
    ];
  }
