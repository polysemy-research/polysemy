{
  system,
  nixpkgs,
  name,
  compiler,
}:
let
  pkgs = import nixpkgs { inherit system; };
  hlib = pkgs.haskell.lib;

  overrides = self: super:
  let
    filter = pkgs.nix-gitignore.gitignoreSourcePure [./source-filter];

    c2n = name: src: self.callCabal2nix name (filter src) {};

    hackage = args: hlib.dontCheck (self.callHackageDirect args {});

  in {
    polysemy = c2n "polysemy" ../.;
    polysemy-plugin = c2n "polysemy-plugin" ../polysemy-plugin;
  } // pkgs.lib.optionalAttrs (name == "912") {
  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
