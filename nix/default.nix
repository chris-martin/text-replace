let

  # Dec 29 2017
  # nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz
  nixpkgs = (import <nixpkgs> { }).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "ade98dc442ea78e9783d5e26954e64ec4a1b2c94";
    sha256 = "1ymyzrsv86mpmiik9vbs60c1acyigwnvl1sx5sd282gndzwcjiyr";
  };

in
  {

    oldest-supported-deps = (import nixpkgs {

      config = {

        packageOverrides = pkgs: let inherit (pkgs) callPackage; in rec {

          haskellPackages = pkgs.haskell.packages.ghc802.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              # Based on Stackage LTS 7.24

              ansi-terminal = haskellPackagesNew.callPackage
                ./ansi-terminal-0-6-3.nix { };

              concurrent-output = haskellPackagesNew.callPackage
                ./concurrent-output-1-7-9.nix { };

              directory = haskellPackagesNew.callPackage
                ./directory-1-2-6-2.nix { };

              HUnit = haskellPackagesNew.callPackage
                ./HUnit-1-3-1-2.nix { };

              neat-interpolation = haskellPackagesNew.callPackage
                ./neat-interpolation-0-3-1-1.nix { };

              optparse-applicative = haskellPackagesNew.callPackage
                ./optparse-applicative-0-12-1-0.nix { };

              parsec = haskellPackagesNew.callPackage
                ./parsec-3-1-11.nix { };

              process = haskellPackagesNew.callPackage
                ./process-1-4-2-0.nix { };

              QuickCheck = haskellPackagesNew.callPackage
                ./QuickCheck-2-8-2.nix { };

              text = haskellPackagesNew.callPackage
                ./text-1-2-2-2.nix { };

              text-replace = haskellPackagesNew.callPackage
                ./text-replace.nix { };

            };
          };
        };
      };
    }).haskellPackages.text-replace;

    ghc-8-0-2 = (import nixpkgs {

      config = {

        packageOverrides = pkgs: let inherit (pkgs) callPackage; in rec {

          haskellPackages = pkgs.haskell.packages.ghc802.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              # Based on Stackage LTS 9.21

              ansi-terminal = haskellPackagesNew.callPackage
                ./ansi-terminal-0-6-3-1.nix { };

              concurrent-output = haskellPackagesNew.callPackage
                ./concurrent-output-1-9-2.nix { };

              process = haskellPackagesNew.callPackage
                ./process-1-4-3-0.nix { };

              text-replace = haskellPackagesNew.callPackage
                ./text-replace.nix { };

            };
          };
        };
      };
    }).haskellPackages.text-replace;

    ghc-8-2-2 = (import nixpkgs {

      config = {

        packageOverrides = pkgs: let inherit (pkgs) callPackage; in rec {

          haskellPackages = pkgs.haskell.packages.ghc822.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              text-replace = haskellPackagesNew.callPackage
                ./text-replace.nix { };

            };
          };
        };
      };
    }).haskellPackages.text-replace;

  }
