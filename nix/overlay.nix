{
  system,
  nixpkgs,
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
  } // pkgs.lib.optionalAttrs (compiler == "ghc910") {

    cabal-doctest = self.callHackageDirect {
      pkg = "cabal-doctest";
      ver = "1.0.10";
      sha256 = "sha256-h6lGtI8czHKiYyoSrrMKrpitidhH5MBfczl9JkJ7Y0E=";
    } {};

    call-stack = hlib.dontCheck super.call-stack;

    doctest = hlib.dontCheck (self.callHackageDirect {
      pkg = "doctest";
      ver = "0.22.2";
      sha256 = "193vrmxcnn9fxn7bc6y7jg8qwr13z9a26qqn0c294mn67il18cqn";
      rev = {
        revision = "1";
        sha256 = "sha256-bZcyn5y1PS27VOv+j4EgIQWRd2aizi5EKs15BoL+NvE=";
      };
    } {});

    ghc-tcplugins-extra = self.callHackageDirect {
      pkg = "ghc-tcplugins-extra";
      ver = "0.4.6";
      sha256 = "sha256-dFCNSkiKP5pr8Kx/p5Cb+WJ4cp/xcQA2Y5JgPwQ0CzQ=";
    } {};

    os-string = null;

    primitive = hlib.dontCheck super.primitive_0_9_0_0;

    th-abstraction = self.callHackageDirect {
      pkg = "th-abstraction";
      ver = "0.7.0.0";
      sha256 = "sha256-YNCvJ9C8PsOTT+B4NoRT6kA2bOFk4F6ygm8hzWMH+1I=";
    } {};

  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
