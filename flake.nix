{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    nixpkgs.url = github:NixOs/nixpkgs/nixos-20.09;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  let
    overlay = import ./nix/overlay.nix;

    pkgs = import nixpkgs {
      inherit system;
      overlays = [overlay];
    };
  in {
    packages = { inherit (pkgs.haskellPackages) polysemy polysemy-plugin; };

    defaultPackage = pkgs.haskellPackages.polysemy;

    devShell = pkgs.haskellPackages.shellFor {
      packages = _: [];
      buildInputs = with pkgs.haskellPackages; [
        cabal-install
        haskell-language-server
        ghcid
      ];
      withHoogle = true;
    };

    checks = {
      inherit (pkgs.haskellPackages) polysemy polysemy-plugin;
    };
  });
}
