{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {nixpkgs, flake-utils, ...}:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  with nixpkgs.lib;
  let
    hsPkgs = nixpkgs: compiler: import ./nix/overlay.nix { inherit system nixpkgs compiler; };

    ghcs = {
      "90" = hsPkgs nixpkgs "ghc90";
      "92" = hsPkgs nixpkgs "ghc92";
      "94" = hsPkgs nixpkgs "ghc94";
      "96" = hsPkgs nixpkgs "ghc96";
      "98" = hsPkgs nixpkgs "ghc98";
      "910" = hsPkgs nixpkgs "ghc910";
      "latest" = hsPkgs nixpkgs "ghc9101";
    };

    default = "96";

    mkPackages = version: {
      "polysemy-${version}" = ghcs.${version}.polysemy;
      "polysemy-plugin-${version}" = ghcs.${version}.polysemy-plugin;
    };

    defaultPackages = {
      inherit (ghcs.${default}) polysemy polysemy-plugin;
      default = ghcs.${default}.polysemy;
    };

    packages = foldl' (l: r: l // r) defaultPackages (map mkPackages (attrNames ghcs));

    mkDevShell = name: ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
      ] ++ nixpkgs.lib.optionals (name != "latest" && name != "910" && name != "98" && name != "90") [
        (ghc.pkgs.haskell.lib.dontCheck ghcid)
        haskell-language-server
      ];
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (mkDevShell n g)) ghcs;

  in {
    inherit packages;

    devShells = devShells // { default = devShells."ghc${default}"; };

    checks = packages;
  });
}
