{ mkDerivation, base, deepseq, filepath, stdenv }:
mkDerivation {
  pname = "HUnit";
  version = "1.3.1.2";
  sha256 = "10akdh4fl615rrshxi3m5gf414il1q42z4zqyb6q4jasmscvzpms";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [ base deepseq filepath ];
  homepage = "https://github.com/hspec/HUnit#readme";
  description = "A unit testing framework for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
