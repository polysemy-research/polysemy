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
    type-errors = pkgs.haskell.lib.doJailbreak super.type-errors;
  } // pkgs.lib.optionalAttrs (compiler == "ghc98") {
    some = pkgs.haskell.lib.doJailbreak super.some;
    type-errors = pkgs.haskell.lib.doJailbreak super.type-errors;
    th-abstraction = self.callHackageDirect {
      pkg = "th-abstraction";
      ver = "0.6.0.0";
      sha256 = "1w07ysxrbjm1rhlg9nhlq5y72s5wr4vqmcy99chvyb56wka0grbq";
    } {};
    tagged = self.callHackageDirect {
      pkg = "tagged";
      ver = "0.8.8";
      sha256 = "1m2bcf0sr1z28gnl2k8xibcsv80kd35816c9c7ji045jbxg27xd9";
    } {};
    hspec = super.hspec_2_11_7;
    hspec-core = super.hspec-core_2_11_7;
    hspec-meta = super.hspec-meta_2_11_7;
    hspec-discover = super.hspec-discover_2_11_7;
    doctest = self.callHackageDirect {
      pkg = "doctest";
      ver = "0.22.2";
      sha256 = "193vrmxcnn9fxn7bc6y7jg8qwr13z9a26qqn0c294mn67il18cqn";
    } {};
  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
