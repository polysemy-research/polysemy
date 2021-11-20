{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    nixpkgs_2009.url = github:nixos/nixpkgs/release-20.09;
    nixpkgs_2105.url = github:nixos/nixpkgs/release-21.05;
    unstable.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs_2009, nixpkgs_2105, unstable, flake-utils, ... }:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  with unstable.lib;
  let
    hsPkgs = nixpkgs: compiler: import ./nix/overlay.nix { inherit system nixpkgs compiler; };

    ghcs = {
      "865" = hsPkgs nixpkgs_2009 "ghc865";
      "884" = hsPkgs nixpkgs_2105 "ghc884";
      "8107" = hsPkgs unstable "ghc8107";
      "901" = hsPkgs unstable "ghc901";
      "921" = hsPkgs unstable "ghc921";
    };

    mkPackages = version: {
      "polysemy-${version}" = ghcs.${version}.polysemy;
      "polysemy-plugin-${version}" = ghcs.${version}.polysemy-plugin;
    };

    packages =
      foldl' (l: r: l // r) { inherit (ghcs."8107") polysemy polysemy-plugin; } (map mkPackages (attrNames ghcs));

    mkDevShell = extra: ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
      ] ++ (if extra then [ghcid haskell-language-server] else []);
      withHoogle = extra;
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (mkDevShell (n != "921") g)) ghcs;

  in {
    inherit packages devShells;

    defaultPackage = packages.polysemy;

    devShell = devShells.ghc8107;

    checks = packages;
  });
}
