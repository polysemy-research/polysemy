{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    unstable.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { unstable, flake-utils, ... }:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  with unstable.lib;
  let
    hsPkgs = nixpkgs: compiler: import ./nix/overlay.nix { inherit system nixpkgs compiler; };

    ghcs = {
      "8107" = hsPkgs unstable "ghc8107";
      "902" = hsPkgs unstable "ghc902";
      "925" = hsPkgs unstable "ghc925";
      "943" = hsPkgs unstable "ghc943";
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

    mkDevShell = ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
        (ghc.pkgs.haskell.lib.dontCheck ghcid)
        haskell-language-server
      ];
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (mkDevShell g)) ghcs;

  in {
    inherit packages;

    devShells = devShells // { default = devShells.ghc902; };

    checks = packages;
  });
}
