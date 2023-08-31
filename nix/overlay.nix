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
  };
in
  pkgs.haskell.packages.${compiler}.override { inherit overrides; } // { inherit pkgs; }
