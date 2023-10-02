let
  config = {
    packageOverrides = pkgs: {
      haskell-language-server = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "96" ];
      };
    };
  };

in
  {
    pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f5892ddac112a1e9b3612c39af1b72987ee5783a.tar.gz") { inherit config; }
  }:

  pkgs.mkShell {
    buildInputs = [
      pkgs.cabal-install
      pkgs.gdb
      pkgs.haskell.compiler.ghc96
      pkgs.haskell-language-server
      pkgs.haskellPackages.hoogle
      pkgs.nasm
    ];
  }
