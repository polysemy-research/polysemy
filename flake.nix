{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    stable.url = github:nixos/nixpkgs/nixos-20.09;
    unstable.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { stable, unstable, flake-utils, ... }:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  let
    hsPkgs = nixpkgs: compiler: import ./nix/overlay.nix { inherit system nixpkgs compiler; };

    ghc865 = hsPkgs stable "ghc865";
    ghc884 = hsPkgs unstable "ghc884";
    ghc8104 = hsPkgs unstable "ghc8104";
    ghc901 = hsPkgs unstable "ghc901";

    packages = {
      inherit (ghc8104) polysemy polysemy-plugin;
      polysemy-865 = ghc865.polysemy;
      polysemy-plugin-865 = ghc865.polysemy-plugin;
      polysemy-884 = ghc884.polysemy;
      polysemy-plugin-884 = ghc884.polysemy-plugin;
      polysemy-8104 = ghc8104.polysemy;
      polysemy-plugin-8104 = ghc8104.polysemy-plugin;
      polysemy-901 = ghc901.polysemy;
      polysemy-plugin-901 = ghc901.polysemy-plugin;
    };
  in {
    inherit packages;

    defaultPackage = ghc8104.polysemy;

    devShell = ghc8104.shellFor {
      packages = _: [];
      buildInputs = with ghc8104; [
        cabal-install
        haskell-language-server
        ghcid
      ];
      withHoogle = true;
    };

    checks = packages;
  });
}
