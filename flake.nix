{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/8b5b7def915305c7d4f5cf236c095bf898bc7995";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {nixpkgs, flake-utils, ...}:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  with nixpkgs.lib;
  let
    hsPkgs = nixpkgs: compiler: import ./nix/overlay.nix { inherit system nixpkgs compiler; };

    ghcs = {
      "810" = hsPkgs nixpkgs "ghc810";
      "90" = hsPkgs nixpkgs "ghc90";
      "92" = hsPkgs nixpkgs "ghc92";
      "94" = hsPkgs nixpkgs "ghc94";
      "96" = hsPkgs nixpkgs "ghc96";
    };

    mkPackages = version: {
      "polysemy-${version}" = ghcs.${version}.polysemy;
      "polysemy-plugin-${version}" = ghcs.${version}.polysemy-plugin;
    };

    defaultPackages = {
      inherit (ghcs."92") polysemy polysemy-plugin;
      default = ghcs."92".polysemy;
    };

    packages = foldl' (l: r: l // r) defaultPackages (map mkPackages (attrNames ghcs));

    mkDevShell = name: ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
      ] ++ nixpkgs.lib.optionals (name != "96") [
        (ghc.pkgs.haskell.lib.dontCheck ghcid)
        haskell-language-server
      ];
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (mkDevShell n g)) ghcs;

  in {
    inherit packages;

    devShells = devShells // { default = devShells.ghc92; };

    checks = packages;
  });
}
