{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/fb942492b7accdee4e6d17f5447091c65897dde4.tar.gz") { }
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.dune_3
    pkgs.ocaml
    pkgs.ocamlPackages.ocaml-lsp
    pkgs.ocamlPackages.utop
  ];
}
