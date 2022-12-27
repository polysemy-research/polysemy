{
  system,
  nixpkgs,
  compiler,
}:
let
  overrides = pkgs: self: super:
  let
    filter = pkgs.nix-gitignore.gitignoreSourcePure [./source-filter];

    c2n = name: src: pkgs.haskell.lib.disableLibraryProfiling (self.callCabal2nix name (filter src) {});
  in {
    polysemy = c2n "polysemy" ../.;
    polysemy-plugin = c2n "polysemy-plugin" ../polysemy-plugin;
  };

  pkgs = import nixpkgs { inherit system; };
in
  pkgs.haskell.packages.${compiler}.override { overrides = overrides pkgs; } // { inherit pkgs; }
