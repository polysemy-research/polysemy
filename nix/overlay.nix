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
  } // pkgs.lib.optionalAttrs (name == "latest" || name == "912") {

    doctest = hackage {
      pkg = "doctest";
      ver = "0.24.0";
      sha256 = "sha256-29IfIrJ4jI+ycwQci+MCdKCEJW1m9DQGxNipvfxmMtI=";
    };

    ghc-tcplugins-extra = hackage {
      pkg = "ghc-tcplugins-extra";
      ver = "0.5";
      sha256 = "sha256-mOzdicJevaXZdZS4/RA1hU3CWJXMFwMUfmEH3YxX4Q8=";
    };

    hashable = hlib.doJailbreak (hackage {
      pkg = "hashable";
      ver = "1.5.0.0";
      sha256 = "sha256-IYAGl8K4Fo1DGSE2kok3HMtwUOJ/mwGHzVJfNYQTAsI=";
    });

    th-abstraction = hackage {
      pkg = "th-abstraction";
      ver = "0.7.1.0";
      sha256 = "sha256-XZ8f1KnMszsFitzN1qsWEOLN09yqhhaR9tn/u1I/mSc=";
    };

  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
