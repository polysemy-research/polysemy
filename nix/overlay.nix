{
  system,
  nixpkgs,
  compiler,
}:
let
  overrides = pkgs: self: super:
  let
    inherit (builtins) splitVersion;
    inherit (pkgs.lib) compare compareLists;
    hs = pkgs.haskell.lib;
    jailbreak = hs.doJailbreak;

    filter = pkgs.nix-gitignore.gitignoreSourcePure [./source-filter];

    c2n = name: src: pkgs.haskell.lib.disableLibraryProfiling (self.callCabal2nix name (filter src) {});
    hackage = pkg: ver: sha256: self.callHackageDirect { inherit pkg ver sha256; } {};

    is92 = compareLists compare (splitVersion self.ghc.version) ["9" "2" "0"] >= 0;
    is94 = compareLists compare (splitVersion self.ghc.version) ["9" "4" "2"] >= 0;
    if92 = n: f: if is92 then f n else n;

    fcf = hackage "first-class-families" "0.8.0.1" "0h1rxbc7zsxrlhx5xcl58wjx3qi2wny8wb3sk7c1qnydf4ckcckz";
  in {
    doctest = hs.unmarkBroken super.doctest_0_20_0;
    cabal-doctest = # hs.unmarkBroken super.doctest_0_20_0;
      if is92
      then hackage "cabal-doctest" "1.0.9" "0irxfxy1qw7sif4408xdhqycddb4hs3hcf6xfxm65glsnmnmwl2i"
      else if is94 then hs.doJailbreak super.cabal-doctest
      else super.cabal-doctest;
    dump-core = hackage "dump-core" "0.1.3.2" "1mi8p736yn00z549pwnjv4ydwbs8mwg6dla3ly447c027nq8py6g";
    first-class-families = if92 fcf jailbreak;
    monadLib = hackage "monadLib" "3.10" "1v4ynjcb963s3lfw3v71qdzvld1mmz1faf8swhvicma5jbvwchy2";
    polysemy = c2n "polysemy" ../.;
    polysemy-plugin = c2n "polysemy-plugin" ../polysemy-plugin;
    primitive = if92 super.primitive jailbreak;
    type-errors = if92 super.type-errors hs.dontCheck;
  };

  pkgs = import nixpkgs { inherit system; };
in
  pkgs.haskell.packages.${compiler}.override { overrides = overrides pkgs; }
