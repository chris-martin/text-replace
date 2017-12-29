let

  # Dec 18 2017
  # nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz
  nixpkgs = (import <nixpkgs> { }).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "afe9649210cace6d3ee9046684d4ea27dc4fd15d";
    sha256 = "19w9cvf8kn369liz3yxab4kam1pbqgn5zlry3832g29w82jwpz2l";
  };

in
  {

    ghc-8-0-2 = (import nixpkgs {

      config = {

        packageOverrides = pkgs: let inherit (pkgs) callPackage; in rec {

          haskellPackages = pkgs.haskell.packages.ghc802.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              text-replace = haskellPackagesNew.callPackage
                ./text-replace.nix { };

            };
          };
        };
      };
    }).haskellPackages.text-replace;

    ghc-8-2-1 = (import nixpkgs {

      config = {

        packageOverrides = pkgs: let inherit (pkgs) callPackage; in rec {

          haskellPackages = pkgs.haskell.packages.ghc821.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              text-replace = haskellPackagesNew.callPackage
                ./text-replace.nix { };

            };
          };
        };
      };
    }).haskellPackages.text-replace;

  }
