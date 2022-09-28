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
      "902" = hsPkgs unstable "ghc902";
      "924" = hsPkgs unstable "ghc924";
    };

    mkPackages = version: {
      "polysemy-${version}" = ghcs.${version}.polysemy;
      "polysemy-plugin-${version}" = ghcs.${version}.polysemy-plugin;
    };

    defaultPackages = {
      inherit (ghcs."902") polysemy polysemy-plugin;
      default = ghcs."902".polysemy;
    };

    packages = foldl' (l: r: l // r) defaultPackages (map mkPackages (attrNames ghcs));

    mkDevShell = extra: ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
      ] ++ (if extra then [ghcid haskell-language-server] else []);
      withHoogle = extra;
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (mkDevShell (n != "924") g)) ghcs;

  in {
    inherit packages;

    devShells = devShells // { default = devShells.ghc902; };

    checks = packages;
  });
}
