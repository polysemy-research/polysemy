{
  system,
  nixpkgs,
  compiler,
}:
let
  pkgs = import nixpkgs { inherit system; };

  overrides = self: super:
  let
    filter = pkgs.nix-gitignore.gitignoreSourcePure [./source-filter];

    c2n = name: src: pkgs.haskell.lib.disableLibraryProfiling (self.callCabal2nix name (filter src) {});

  in {
    polysemy = c2n "polysemy" ../.;
    polysemy-plugin = c2n "polysemy-plugin" ../polysemy-plugin;
  } // pkgs.lib.optionalAttrs (compiler == "ghc96") {
    type-errors = self.callHackageDirect {
      pkg = "type-errors";
      ver = "0.2.0.2";
      sha256 = "sha256-z/QANg5jUhSc02IBVQEsg7QemiyV3XdwxN9K1CH2Myc=";
    } {};
  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
