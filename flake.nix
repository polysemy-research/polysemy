{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {nixpkgs, flake-utils, ...}:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  let
    inherit (nixpkgs) lib;

    hsPkgs = nixpkgs: name: compiler: import ./nix/overlay.nix { inherit system name nixpkgs compiler; };

    conf = {
      "90" = "ghc90";
      "92" = "ghc92";
      "94" = "ghc94";
      "96" = "ghc96";
      "98" = "ghc98";
      "910" = "ghc910";
      "latest" = "ghc9101";
    };

    ghcs = lib.mapAttrs (hsPkgs nixpkgs) conf;

    default = "96";

    mkPackages = version: {
      "polysemy-${version}" = ghcs.${version}.polysemy;
      "polysemy-plugin-${version}" = ghcs.${version}.polysemy-plugin;
    };

    defaultPackages = {
      inherit (ghcs.${default}) polysemy polysemy-plugin;
      default = ghcs.${default}.polysemy;
    };

    packages = lib.foldl' (l: r: l // r) defaultPackages (map mkPackages (lib.attrNames ghcs));

    mkDevShell = name: ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
      ] ++ nixpkgs.lib.optionals (name != "latest" && name != "910" && name != "98" && name != "90") [
        (ghc.pkgs.haskell.lib.dontCheck ghcid)
        haskell-language-server
      ];
    };

    devShells = lib.mapAttrs' (n: g: lib.nameValuePair "ghc${n}" (mkDevShell n g)) ghcs;

  in {
    inherit packages;

    devShells = devShells // { default = devShells."ghc${default}"; };

    checks = packages;
  });
}
