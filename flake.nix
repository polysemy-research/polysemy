{
  description = "Higher-order, low-boilerplate free monads.";

  inputs = {
    nixpkgs_2009.url = "github:nixos/nixpkgs/release-20.09";
    nixpkgs_2105.url = "github:nixos/nixpkgs/release-21.05";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    doctest = {
      url = "github:sol/doctest/ghc-9.4";
      flake = false;
    };
  };

  outputs = { self, nixpkgs_2009, nixpkgs_2105, unstable, flake-utils, ... }:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  with unstable.lib;
  let

    hsPkgs = nixpkgs: compiler: 
      let
        pkgs = import nixpkgs {
          inherit system; 
          overlays =  [ self.overlays."${compiler}-overlay" ];
        };
      in
      # import ./nix/overlay.nix { inherit system compiler; };
      pkgs.polysemyHaskellPackages;
      # .extend(self.overlays."${compiler}-overlay");


    ghcs = {
      "865" = hsPkgs nixpkgs_2009 "ghc865";
      "884" = hsPkgs nixpkgs_2105 "ghc884";
      "8107" = hsPkgs unstable "ghc8107";
      "902" = hsPkgs unstable "ghc902";
      "924" = hsPkgs unstable "ghc924";
      "942" = hsPkgs unstable "ghc942";
    };

    mkPackages = version: {
      "polysemy-${version}" = ghcs.${version}.polysemy;
      "polysemy-plugin-${version}" = ghcs.${version}.polysemy-plugin;
    };

    defaultPackages = {
      inherit (ghcs."902") polysemy polysemy-plugin;
      default = ghcs."902".polysemy;
    };

    packages = foldl' (l: r: l // r) defaultPackages (map mkPackages (attrNames ghcs));

    mkDevShell = extra: ghc: ghc.shellFor {
      packages = p: [p.polysemy p.polysemy-plugin];
      buildInputs = with ghc; [
        cabal-install
      ] ++ (if extra then [
        ghcid 
        # haskell-language-server
      ] else []);
      withHoogle = extra;
    };

    devShells = mapAttrs' (n: g: nameValuePair "ghc${n}" (mkDevShell (n != "924") g)) ghcs;

  in {
    inherit packages;

    devShells = devShells // { default = devShells.ghc942; };

    checks = packages;
  }) // {

    overlays = {
        ghc942-overlay = final: prev: let
          filter = final.nix-gitignore.gitignoreSourcePure [./nix/source-filter];


          # is92 = compareLists compare (splitVersion self.ghc.version) ["9" "2" "0"] >= 0;
          # is94 = compareLists compare (splitVersion self.ghc.version) ["9" "4" "2"] >= 0;
          # if92 = n: f: if is92 then f n else n;

        in {
          polysemyHaskellPackages = prev.haskell.packages.ghc942.extend(hfinal: hprev: with prev.haskell.lib; let 
            hackage = pkg: ver: sha256: hfinal.callHackageDirect { inherit pkg ver sha256; } {};
            fcf = hackage "first-class-families" "0.8.0.1" "0h1rxbc7zsxrlhx5xcl58wjx3qi2wny8wb3sk7c1qnydf4ckcckz";
            c2n = name: src: final.haskell.lib.disableLibraryProfiling (hfinal.callCabal2nix name (filter src) {});

          in {
            doctest = dontCheck (overrideSrc hprev.doctest_0_20_0 { src = self.inputs.doctest; }); # doJailbreak hold.doctest_0_20_0;

            cabal-doctest = doJailbreak hprev.cabal-doctest;
              # if is92
              # then hackage "cabal-doctest" "1.0.9" "0irxfxy1qw7sif4408xdhqycddb4hs3hcf6xfxm65glsnmnmwl2i"
              # else if is94 then hs.doJailbreak super.cabal-doctest
              # else super.cabal-doctest;
            dump-core = hackage "dump-core" "0.1.3.2" "1mi8p736yn00z549pwnjv4ydwbs8mwg6dla3ly447c027nq8py6g";
            first-class-families = doJailbreak fcf ;
            monadLib = hackage "monadLib" "3.10" "1v4ynjcb963s3lfw3v71qdzvld1mmz1faf8swhvicma5jbvwchy2";
            polysemy = c2n "polysemy" ./.;
            polysemy-plugin = c2n "polysemy-plugin" ./polysemy-plugin;
            primitive = doJailbreak hprev.primitive ;
            type-errors = dontCheck hprev.type-errors;
            ghc-source-gen = doJailbreak hprev.ghc-source-gen;

            hspec-contrib = dontCheck hprev.hspec-contrib;
          });
      };
    };

  };
}
