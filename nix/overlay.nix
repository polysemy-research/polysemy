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

  in {
    polysemy = c2n "polysemy" ../.;
    polysemy-plugin = c2n "polysemy-plugin" ../polysemy-plugin;
  } // pkgs.lib.optionalAttrs (name == "latest") {

    doctest = hlib.dontCheck (self.callHackageDirect {
      pkg = "doctest";
      ver = "0.24.0";
      sha256 = "sha256-29IfIrJ4jI+ycwQci+MCdKCEJW1m9DQGxNipvfxmMtI=";
    } {});

  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
