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
    hackage = pkg: ver: sha256: self.callHackageDirect { inherit pkg ver sha256; } {};
  in {
    polysemy = c2n "polysemy" ../.;
    polysemy-plugin = c2n "polysemy-plugin" ../polysemy-plugin;
    first-class-families =
      hackage "first-class-families" "0.8.0.1" "0h1rxbc7zsxrlhx5xcl58wjx3qi2wny8wb3sk7c1qnydf4ckcckz";
    dump-core = hackage "dump-core" "0.1.3.2" "1mi8p736yn00z549pwnjv4ydwbs8mwg6dla3ly447c027nq8py6g";
    monadLib = hackage "monadLib" "3.10" "1v4ynjcb963s3lfw3v71qdzvld1mmz1faf8swhvicma5jbvwchy2";
  };

  pkgs = import nixpkgs { inherit system; };
in
  pkgs.haskell.packages.${compiler}.override { overrides = overrides pkgs; }
